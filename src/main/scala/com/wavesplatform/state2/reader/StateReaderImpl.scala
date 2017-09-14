package com.wavesplatform.state2.reader

import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Address, Alias}
import scorex.transaction.{Transaction, TransactionParser}

import scala.collection.JavaConverters._

class StateReaderImpl(p: StateStorage) extends StateReader {

  override def leaseInfo(a: Address) = ???

  override def leaseDetails(leaseId: ByteStr) = ???

  override def wavesBalance(a: Address) = ???

  override def assetBalance(a: Address) = ???

  override def nonZeroLeaseBalances = ???

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = {
    Option(p.transactions.get(id)).map {
      case (h, bytes) => (h, TransactionParser.parseBytes(bytes).get)
    }
  }

  override def assetInfo(id: ByteStr): Option[AssetInfo] = {
    Option(p.assets.get(id)).map {
      case (is, amt) => AssetInfo(is, amt)
    }
  }

  override def assetDescription(id: ByteStr) = ???

  override def height: Int = { p.getHeight }

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = {
    val totalRecords = p.accountTransactionsLengths.getOrDefault(a.bytes, 0)
    Range(Math.max(0, totalRecords - limit), totalRecords)
      .map(n => p.accountTransactionIds.get(StateStorage.accountIndexKey(a, n)))
      .reverse
  }

  override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr] = {
    Option(p.paymentTransactionHashes.get(hash))
  }

  override def aliasesOfAddress(a: Address): Seq[Alias] = {
    p.aliasToAddress.asScala
      .collect { case (aliasName, addressBytes) if addressBytes == a.bytes =>
        Alias.buildWithCurrentNetworkByte(aliasName).explicitGet()
      }.toSeq
  }

  override def resolveAlias(a: Alias): Option[Address] = {
    Option(p.aliasToAddress.get(a.name))
      .map(b => Address.fromBytes(b.arr).explicitGet())
  }

  override def activeLeases: Seq[ByteStr] = {
    p.leaseState
      .asScala
      .collect { case (leaseId, isActive) if isActive => leaseId }
      .toSeq
  }

  override def lastUpdateHeight(acc: Address): Option[Int] =  {
    Option(p.lastBalanceSnapshotHeight.get(acc.bytes))
  }

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] =  {
    Option(p.balanceSnapshots.get(StateStorage.accountIndexKey(acc, h)))
      .map { case (ph, b, eb) => Snapshot(ph, b, eb) }
  }

  override def containsTransaction(id: ByteStr): Boolean = {
    p.transactions.containsKey(id)
  }

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo = {
    Option(p.orderFills.get(orderId)).map(oi => OrderFillInfo(oi._1, oi._2)).orEmpty
  }
}
