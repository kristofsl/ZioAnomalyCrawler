import model.Model
import model.Model.{AnomalyDetectionInputFeatureRecord, AnomalyDetectionResult}
import play.api.libs.json.{JsString, Json, __}
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderFailures
import zio.blocking.Blocking
import zio.clock.Clock
import zio.duration.durationInt
import zio.kafka.consumer.{Consumer, ConsumerSettings, _}
import zio.kafka.serde.Serde
import pureconfig._
import pureconfig.generic.auto._
import services.IsolationForest
import services.IsolationForest.IsolationForestEnv
import zio.{UIO, ZIO, ZLayer, ZManaged, _}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object ZioCrawler extends scala.App {

  println("Booting up the ZIO anomaly detection crawler...")

  /** The application configuration types that are configured via the application.conf file */
  case class ServiceConf(crawlerConf: Config)
  case class Config(bootstrapServer: String, kafkaGroupId: String, kafkaConsumerId: String, inputTopic: String, idField: IdField, anomalyFields: List[AnomalyField])
  case class AnomalyField(fieldLabel: String)
  case class IdField(label: String)

  /** read the system environment properties that are passed in via docker run command or kubernetes and make it safe with defaults */
  val sysEnv: Map[String, String] = sys.env

  val batchSize: Int =  scala.util.Properties.envOrElse(Model.kafkaBatchSize, "1000").toInt
  val batchSecondsLimit: Int = scala.util.Properties.envOrElse(Model.kafkaBatchWindowLimit, "1000").toInt

  val rt = Runtime.default

  /** load the startup configurations */
  val applicationConfig: Either[ConfigReaderFailures, ServiceConf] = ConfigSource.default.load[ServiceConf]

  /** the isolation forest layer */
  val isolationForestLive: ZLayer[Any, Throwable, IsolationForestEnv] = IsolationForest.live

  /** build the live kafka environment settings */
  def buildKafkaLiveConsumerEnv(bootstrapServer: String, groupId: String, consumerId: String): ZLayer[Clock with Blocking, Throwable, Consumer] = {
    val consumerSettings: ConsumerSettings = ConsumerSettings(List(bootstrapServer))
      .withGroupId(groupId)
      .withCloseTimeout(60.seconds)
      .withClientId(consumerId)
      .withProperty("enable.auto.commit", "false")
      .withProperty("security.protocol", "SASL_SSL")
      .withProperty("sasl.mechanism", "PLAIN")
      .withProperty("auto.offset.reset", "earliest")

    val consumerManaged: ZManaged[Clock with Blocking, Throwable, Consumer.Service] =
      Consumer.make(consumerSettings)

    ZLayer.fromManaged(consumerManaged)
  }

  /** the main kafka processing loop that takes X records or waits for Y seconds and then collects */
  def kafkaProcessingGroupedWithinLoop(topic: String, processingLogic: (Chunk[CommittableRecord[String, String]],
                          List[AnomalyField], IdField) => Task[Unit],
                                       anomalyFields: List[AnomalyField],
                                       idField: IdField): ZIO[Any with Clock with Blocking with Consumer, Throwable, Unit] = {
    for {
      _ <- ZIO.effect(println("Starting the kafka topic subscription using the kafkaProcessingGroupedWithinLoop .."))
      _ <- Consumer.subscribeAnd(Subscription.topics(topic))
        .plainStream(Serde.string, Serde.string)
        .groupedWithin(batchSize, batchSecondsLimit.seconds)
        .mapM { batch =>
          processingLogic(batch, anomalyFields, idField) *>
            batch.map(_.offset)
              .foldLeft(OffsetBatch.empty)(_ merge _)
              .commit
        }
        .runDrain
        .foldM(err => UIO(println(s"ERROR ${err.getMessage}")).as(1), _ => UIO.succeed(0))
    } yield ()
  }

  def getCurrentTime(): String = {
    val dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss")
    LocalDateTime.now().format(dtf)
  }



  /** The big processing function that processes a list of records */
  def processRecords(value: Chunk[CommittableRecord[String, String]], anomalyFields: List[AnomalyField], idField: IdField): Task[Unit] = {
    for {
      _                                                     <- Task.effect(println(s"Anomaly detection for window started on ${getCurrentTime()}..."))
      d: Chunk[String]                                      <- ZIO.effect(value.map(_.value))
      input: Chunk[AnomalyDetectionInputFeatureRecord]      <- ZIO.foreachPar(d) {
                                                              (jsonValue) => {
                                                                for {
                                                                  id:String                             <- ZIO.getOrFail(findJsonValues(jsonValue, idField.label).headOption)
                                                                  anomalyFields:List[Double]            <- ZIO.foreachPar(anomalyFields) {
                                                                                                            (aField) => {
                                                                                                              Task.effect {
                                                                                                                val values = {
                                                                                                                  findJsonValues(jsonValue, aField.fieldLabel).map(_.toDouble)
                                                                                                                }
                                                                                                                if (values != 0 && values.length > 0)  values.sum / values.length else 0
                                                                                                              }.orElse(Task.succeed(0.0))
                                                                                                            }
                                                                                                          }
                                                                } yield (AnomalyDetectionInputFeatureRecord(id, anomalyFields))
                                                              }
                                                            }
      results: List[AnomalyDetectionResult]                 <- IsolationForest.anomalyDetection(anomalyFields.map(_.fieldLabel), input.toList, 200, 100, 50, None).provideLayer(isolationForestLive)
      _                                                     <- Task.effect(println(s"Anomaly detection for window finished on ${getCurrentTime()}..."))
      posResults : List[AnomalyDetectionResult]             <- UIO.effectTotal(results.filter(_.anomaly))
      _                                                     <- Task.effect(println(s"Window positive anomaly results : ${posResults.map(_.inputRecord.input).mkString("-")}"))
      } yield()
  }

  /** recursive search in the json tree trying to find a list a label matches */
  def findJsonValues(v:String, label:String):List[String] = {
    try {
      (Json.parse(v) \\ label).map(jsValue => jsValue.as[String]).toList
    } catch {
      case _ : Throwable => List()
    }
  }

  /** start the processing using the application config (application.conf file in resources directory) */
  applicationConfig match {
    case Left(ex) => {
      println(s"Invalid configuration ... ${ex.toList.mkString("-")}")
    }
    case Right(config) => {
      rt.unsafeRun(
        kafkaProcessingGroupedWithinLoop(config.crawlerConf.inputTopic,processRecords,config.crawlerConf.anomalyFields, config.crawlerConf.idField).provideCustomLayer(buildKafkaLiveConsumerEnv(config.crawlerConf.bootstrapServer, config.crawlerConf.kafkaGroupId, config.crawlerConf.kafkaConsumerId) ++ isolationForestLive)
      )
    }
  }
}
