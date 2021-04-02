import model.Model.AnomalyDetectionInputFeatureRecord
import org.scalatest.funsuite.AnyFunSuite
import services.{IsolationForest, Logging}
import zio.{Has, Task, ZLayer}
import zio._

import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.util.Random

class IsolationForestGridSearch extends AnyFunSuite {

  def generateAnomalyDetectionInputOneFeatureRecord(start: Int, end: Int) = {
    AnomalyDetectionInputFeatureRecord(UUID.randomUUID().toString,List(Random.between(start + 0.00000001, end)))
  }

  /** not a real test but a hyper parameter grid search method for experimentation */
  test(testName = "grid_search") {
    var buffer:ListBuffer[AnomalyDetectionInputFeatureRecord] = new ListBuffer[AnomalyDetectionInputFeatureRecord]
    for(x <- 1 to 1000) {
      buffer.append(generateAnomalyDetectionInputOneFeatureRecord(-60,-30))
    }
    buffer.append(AnomalyDetectionInputFeatureRecord("X",List(-200)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X",List(-300)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X",List(-200)))
    for(x <- 1 to 1000) {
      buffer.append(generateAnomalyDetectionInputOneFeatureRecord(-60,-30))
    }
    buffer.append(AnomalyDetectionInputFeatureRecord("X",List(-1)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X",List(-1)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X",List(-8)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X",List(-4)))

    val layer: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = IsolationForest.live

    val r = for {
      r <- IsolationForest.anomalyDetection(List("feature_x"), buffer.toList, 200, 50, 30, Some(1))
      _ <- ZIO.foreach(r) {
        r => Task.effect(println(s"${r.recordKey} ${r.presentTreeCount} ${r.inputRecord.input.head} ${r.avgDepth} ${r.anomalyScore} ${r.anomaly}"))
      }
    } yield()

    val rt = Runtime.default
    rt.unsafeRun(
      r.provideLayer(layer)
    )
  }

}
