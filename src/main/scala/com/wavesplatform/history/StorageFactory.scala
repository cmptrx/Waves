package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}
import javax.sql.DataSource

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriter, StateWriterImpl}
import scorex.transaction.{BlockchainUpdater, History}

import scala.util.{Success, Try}

object StorageFactory {

  private def createStateStorage(history: History, stateFile: Option[File]): Try[StateStorage] =
    StateStorage(stateFile, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true)
      }
    }

  def apply(settings: BlockchainSettings, ds: DataSource): Try[(History with AutoCloseable, StateWriter, StateReader, BlockchainUpdater)] = {
    val lock = new RWL(true)

    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainFile, lock)
      ss <- createStateStorage(historyWriter, settings.stateFile)
      stateWriter = new StateWriterImpl(ds)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, lock)
      (historyWriter, stateWriter, bcu.currentPersistedBlocksState, bcu)
    }
  }
}
