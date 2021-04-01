import model.Model.{AnomalyDetectionInputFeatureRecord, AnomalyDetectionResult}
import services.IsolationForest
import zio.test.Assertion._
import zio.test._
import zio.{Has, Task, ZIO, ZLayer}
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.util.Random


object IsolationForestSpecBasicSetTwoFeatures extends DefaultRunnableSpec {

  def generateAnomalyDetectionInputTwoFeaturesRecord(start1: Int, end1: Int, start2: Int, end2: Int) = {
    AnomalyDetectionInputFeatureRecord(UUID.randomUUID().toString,List(Random.between(start1 + 0.00000001, end1),Random.between(start2 + 0.00000001, end2)))
  }

  def spec = suite("IsolationForest_basic_set_two_features")(

    /** a simple scenario with two feature : This is non deterministic so we choose tests that are always giving good results **/
    testM("simple_scenario_two_features") {
      val inputFeatureRecord = AnomalyDetectionInputFeatureRecord("X", List(1,2))
      val layer: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = IsolationForest.live

      assertM(
        IsolationForest.anomalyDetection(List("feature_1","feature_2"), List(inputFeatureRecord), 100, 20, 50, Some(1)).provideLayer(layer)
      )(equalTo(
        List(AnomalyDetectionResult(
        labels = List("feature_1","feature_2"),
        recordKey = "X",
        inputRecord = inputFeatureRecord,
        avgDepth = 0.0,
        anomalyScore = 0.0,
        presentTreeCount = 100,
        anomaly = false,
      ))))
    },
    testM("10_records") {
      val inputFeatureRecord1 = AnomalyDetectionInputFeatureRecord("X1", List(1,2))
      val inputFeatureRecord2 = AnomalyDetectionInputFeatureRecord("X2", List(1,2))
      val inputFeatureRecord3 = AnomalyDetectionInputFeatureRecord("X3", List(1,2))
      val inputFeatureRecord4 = AnomalyDetectionInputFeatureRecord("X4", List(1,2))
      val inputFeatureRecord5 = AnomalyDetectionInputFeatureRecord("X5", List(1,2))
      val inputFeatureRecord6 = AnomalyDetectionInputFeatureRecord("X6", List(1,2))
      val inputFeatureRecord7 = AnomalyDetectionInputFeatureRecord("X7", List(1,2))
      val inputFeatureRecord8 = AnomalyDetectionInputFeatureRecord("X8", List(1,2))
      val inputFeatureRecord9 = AnomalyDetectionInputFeatureRecord("X9", List(1,2))
      val inputFeatureRecord10 = AnomalyDetectionInputFeatureRecord("X10", List(1,2))
      val layer: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = IsolationForest.live

      assertM(
        IsolationForest.anomalyDetection(List("feature_1","feature_2"), List(
          inputFeatureRecord1,
          inputFeatureRecord2,
          inputFeatureRecord3,
          inputFeatureRecord4,
          inputFeatureRecord5,
          inputFeatureRecord6,
          inputFeatureRecord7,
          inputFeatureRecord8,
          inputFeatureRecord9,
          inputFeatureRecord10
        ), 100, 20, 50, Some(1)).provideLayer(layer).map(f => f.map(_.anomaly))
      )(equalTo(
        List(
            false,false,false,false,false,false,false,false,false, false
          )
        ))
    }
  )
}


