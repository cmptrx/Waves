package controller

import java.util.logging.Logger

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import api.http.HttpServiceActor
import scorex.block.BlockchainController
import scorex.database.UnconfirmedTransactionsDatabaseImpl
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.network.NetworkController
import scorex.network.message._
import scorex.transaction.Transaction
import scorex.wallet.Wallet
import settings.{Constants, Settings}
import spray.can.Http


object Controller {
  private implicit lazy val actorSystem = ActorSystem("lagonaki")

  lazy val networkController = actorSystem.actorOf(Props[NetworkController])
  lazy val blockchainController = actorSystem.actorOf(Props(classOf[BlockchainController], networkController))

  private lazy val walletFile = new java.io.File(Settings.walletDir, "wallet.s.dat")
  lazy val wallet = new Wallet(walletFile, Settings.walletPassword, Settings.walletSeed)

  lazy val blockchainStorage = new PrunableBlockchainStorage(Some(Settings.dataDir))

  def init() {
    if (blockchainStorage.isEmpty) {
      val genesisBlock = Constants.ConsensusAlgo.genesisBlock
      genesisBlock.process()
      blockchainStorage.appendBlock(genesisBlock)
    }.ensuring(blockchainStorage.height() >= 1)

    val httpServiceActor = actorSystem.actorOf(Props[HttpServiceActor], "http-service")
    val bindCommand = Http.Bind(httpServiceActor, interface = "0.0.0.0", port = Settings.rpcPort)
    IO(Http) ! bindCommand

    blockchainController ! BlockchainController.CheckState //just to init lazy val

    //CLOSE ON UNEXPECTED SHUTDOWN
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        stopAll()
      }
    })
  }

  //todo: stopping the app works bad
  def stopAll() = this.synchronized {
    Logger.getGlobal.info("Stopping message processor")
    networkController ! NetworkController.ShutdownNetwork

    Logger.getGlobal.info("Stopping actors (incl. block generator)")
    actorSystem.shutdown()

    //CLOSE WALLET
    Logger.getGlobal.info("Closing wallet")
    wallet.close()

    //FORCE CLOSE
    System.exit(10)
  }

  def onNewOffchainTransaction(transaction: Transaction) =
    if (UnconfirmedTransactionsDatabaseImpl.putIfNew(transaction)) {
      networkController ! NetworkController.BroadcastMessage(TransactionMessage(transaction))
    }
}