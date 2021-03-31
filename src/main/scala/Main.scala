
import model.Model.AnomalyDetectionInputFeatureRecord
import services.IsolationForest.IsolationForestEnv
import services.Logging.{LoggingEnv, logError, logInfo}
import services.{IsolationForest, Logging}
import zio._

object Main extends scala.App {

  /** our Zlayer defintion */
  val backendLive:ZLayer[Any, Throwable, LoggingEnv with IsolationForestEnv] = Logging.live ++ IsolationForest.live

  /** our main program */
  val mainProgram : ZIO[Has[Logging.Service] with Has[IsolationForest.Service], Throwable, Unit] = for {
    _         <- Logging.logInfo("Starting up ...")
    result    <- IsolationForest.anomalyDetection(List("label"), List(AnomalyDetectionInputFeatureRecord(
                    "key", List(1)
                  )), 1, 1, 1, Some(1))
    _         <- Logging.logInfo(result.mkString("-"))
    _         <- Logging.logInfo("Finished ...")
  } yield()

  val rt = Runtime.default
  rt.unsafeRun(
    mainProgram.provideLayer(backendLive)
  )

}