import zio.{ExitCode, UIO, ZIO, ZLayer, ZManaged}
import zio.kafka.consumer.{Consumer, ConsumerSettings}
import zio.blocking.Blocking
import zio.clock.Clock
import zio.kafka.consumer._
import zio.kafka.serde.Serde
import zio._
import zio.duration.durationInt

object MyApp extends App {

  val consumerSettings: ConsumerSettings = ConsumerSettings(List(""))
    .withGroupId("ZioAnomalyCrawler")
    .withCloseTimeout(30.seconds)
    .withClientId("ZioAnomalyCrawler")
    .withProperty("enable.auto.commit", "false")
    .withProperty("security.protocol", "SASL_SSL")
    .withProperty("sasl.mechanism", "PLAIN")
    .withProperty("auto.offset.reset", "earliest")

  val consumerManaged: ZManaged[Clock with Blocking, Throwable, Consumer.Service] =
    Consumer.make(consumerSettings)
  val consumer: ZLayer[Clock with Blocking, Throwable, Consumer] =
    ZLayer.fromManaged(consumerManaged)

  def logic(): ZIO[zio.ZEnv, Throwable, Unit] = {
    for {
      _   <- ZIO.effect(println("Starting the kafka topic subscription .."))
      _   <- Consumer.subscribeAnd(Subscription.topics("AS400_COMPLETION_EVENT_LIVE_TOPIC"))
            .plainStream(Serde.string, Serde.string)
            .groupedWithin(1000, 30.seconds)
            .mapM { batch =>
              processRecords(batch) *>
                batch.map(_.offset)
                  .foldLeft(OffsetBatch.empty)(_ merge _)
                  .commit
            }
            .runDrain
            .fork
            .provideCustomLayer(consumer)
            .foldM(err => UIO(println(err)).as(1), _ => UIO.succeed(0))
    } yield()
  }

  def processRecords(value: Chunk[CommittableRecord[String, String]]): Task[Unit]= {
    ZIO.succeed()
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = logic().exitCode
}
