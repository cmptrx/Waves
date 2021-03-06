package com.wavesplatform.state2

import com.wavesplatform.history.HistoryWriterImpl
import scorex.lagonaki.mocks.{TestBlock, TestBlock3}
import scorex.transaction.TransactionParser.SignatureLength

trait HistoryTest {
  def appendGenesisBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(TestBlock.withReference(ByteStr(Array.fill(SignatureLength)(0: Byte))))(Right(BlockDiff.empty)).explicitGet()

  def appendTestBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(TestBlock.withReference(history.lastBlock.get.uniqueId))(Right(BlockDiff.empty)).explicitGet()

  def appendTestBlock3(history: HistoryWriterImpl, features: Set[Short]): Unit =
    history.appendBlock(TestBlock3.withReferenceAndFeatures(history.lastBlock.get.uniqueId, features))(Right(BlockDiff.empty)).explicitGet()
}
