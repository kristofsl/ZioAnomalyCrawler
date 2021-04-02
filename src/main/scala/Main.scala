
import model.Model.{AnomalyDetectionInputFeatureRecord, AnomalyDetectionResult}
import services.IsolationForest.IsolationForestEnv
import services.Logging.{LoggingEnv, logError, logInfo}
import services.{IsolationForest, Logging}
import zio.{Queue, UIO, _}

object MinimalApplication extends cask.MainRoutes{
  val rt = Runtime.default

  @cask.get("/")
  def hello() = {
    "Server is responding ..."
  }


  @cask.postJson("/anomaly_set_one_dimension")
  def anomalyCrawlOnOneDimensionalSet(name: ujson.Value, values: ujson.Value):String = {
    rt.unsafeRun(
      handleOneDimensionalHttpRequest.handleRequest(name.str, values.arr.map(_.num).toList).map(r => "OK")
    )
  }

  initialize()
}

object handleOneDimensionalHttpRequest {
  val backendLive:ZLayer[Any, Throwable, LoggingEnv with IsolationForestEnv] = Logging.live ++ IsolationForest.live

  def handleRequest(name: String, values:List[Double]): ZIO[Any, Throwable, Unit]= {
      for {
        input:List[AnomalyDetectionInputFeatureRecord] <- ZIO.effect(values.map(v => new AnomalyDetectionInputFeatureRecord("key",List(v))))
        result                                         <- IsolationForest.anomalyDetection(List("feature_1"), input,100,50,50).provideLayer(backendLive)
        _                                              <- Logging.logInfo(result.filter(_.anomaly).map(a => (a.inputRecord.input.head,a.anomalyScore,a.avgDepth)).mkString("-")).provideLayer(backendLive)
      } yield()
  }
}