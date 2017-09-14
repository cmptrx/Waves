package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import cats.kernel.Semigroup
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.Block
import scorex.transaction.{Signed, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.SortedMap
import scala.util.Try

object BlockDiffer extends ScorexLogging {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings, s: StateReader,  pervBlockTimestamp : Option[Long])(block: Block): Either[ValidationError, BlockDiff] =
    Signed.validateSignatures(block).flatMap { _ => apply(settings, s, pervBlockTimestamp)(block.feesDistribution, block.timestamp, block.transactionData, 1) }

  def unsafeDiffMany(settings: FunctionalitySettings, s: StateReader, prevBlockTimestamp: Option[Long])(blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft((Monoid[BlockDiff].empty, prevBlockTimestamp)) { case ((diff, prev), block) =>
      val blockDiff = fromBlock(settings, new CompositeStateReader(s, diff), prev)(block).explicitGet()
      (Monoid[BlockDiff].combine(diff, blockDiff), Some(block.timestamp))
    }._1

  private def apply(settings: FunctionalitySettings, s: StateReader, pervBlockTimestamp : Option[Long])(feesDistribution: Diff, timestamp: Long, txs: Seq[Transaction], heightDiff: Int) = {
    val currentBlockHeight = s.height + 1

    val txDiffer = TransactionDiffer(settings, pervBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = txs.foldLeft(right(feesDistribution)) { case (ei, tx) => ei.flatMap(diff =>
      txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
        .map(newDiff => diff.combine(newDiff)))
    }

    txsDiffEi.map { d =>
      val diff = if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
        Monoid.combine(d, LeasePatch(new CompositeStateReader(s, d.asBlockDiff)))
      else d

      implicit val g: Semigroup[Map[ByteStr, Long]] = (x: Map[ByteStr, Long], y: Map[ByteStr, Long]) =>
        x.keySet.map { k =>
          val sum = safeSum(x.getOrElse(k, 0L), y.getOrElse(k, 0L))
          require(sum >= 0, s"Negative balance $sum for asset X'${BigInt(k.arr).toString(16)}', available: ${y.getOrElse(k, 0L)}")
          k -> sum
        }.toMap

      val newSnapshots = diff.portfolios
        .collect { case (acc, portfolioDiff) =>
          val oldPortfolio = s.wavesBalance(acc)
          acc -> SortedMap(currentBlockHeight -> Snapshot(
            prevHeight = currentBlockHeight,
            balance = oldPortfolio.regularBalance + portfolioDiff.balance,
            effectiveBalance = oldPortfolio.effectiveBalance + portfolioDiff.effectiveBalance,
            assetBalances = if (portfolioDiff.assets.isEmpty) Map.empty
              else Semigroup.combine(portfolioDiff.assets, s.assetBalance(acc))(g)
            ))
        }
      BlockDiff(diff, heightDiff, newSnapshots)
    }
  }
}
