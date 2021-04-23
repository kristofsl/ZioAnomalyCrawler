import model.Model.{AnomalyDetectionInputFeatureRecord, AnomalyDetectionResult}
import services.IsolationForest
import zio.test.Assertion._
import zio.test._
import zio.{Has, ZLayer}


object IsolationForestSpecBasicSetOneFeature extends DefaultRunnableSpec {

  def spec = suite("IsolationForest_basic_set_one_feature")(
    /** a simple scenario with one feature and one record */
    testM("simple_one_feature_one_record") {
      val inputFeatureRecord = AnomalyDetectionInputFeatureRecord("key", List(1))
      val layer: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = IsolationForest.live

      IsolationForest.anomalyDetection(List("label"), List(inputFeatureRecord), 100, 20, 50, Some(1)).provideLayer(layer)
        .map(n => assert(n)(equalTo(List(AnomalyDetectionResult(
          labels = List("label"),
          recordKey = "key",
          inputRecord = inputFeatureRecord,
          avgDepth = 0.0,
          anomalyScore = 0.0,
          presentTreeCount = 100,
          anomaly = false,
        )))))
    },

    /** a simple scenario with one feature and the same values for every record (no anomalies) */
    testM("happy_path_one_feature_no_anomalies") {
      val inputFeatureRecord_1 = AnomalyDetectionInputFeatureRecord("1", List(1))
      val inputFeatureRecord_2 = AnomalyDetectionInputFeatureRecord("2", List(1))
      val inputFeatureRecord_3 = AnomalyDetectionInputFeatureRecord("3", List(1))
      val inputFeatureRecord_4 = AnomalyDetectionInputFeatureRecord("4", List(1))
      val layer: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = IsolationForest.live

      IsolationForest.anomalyDetection(List("feature_1"), List(inputFeatureRecord_1, inputFeatureRecord_2, inputFeatureRecord_3, inputFeatureRecord_4), 50, 50, 100, Some(1)).provideLayer(layer)
        .map(n => assert(n)(equalTo(
          List(
            AnomalyDetectionResult(
              labels = List("feature_1"),
              recordKey = "1",
              inputRecord = inputFeatureRecord_1,
              avgDepth = 2.3516559071362195,
              anomalyScore = 0.5,
              presentTreeCount = 50,
              anomaly = false,
            ),
            AnomalyDetectionResult(
              labels = List("feature_1"),
              recordKey = "2",
              inputRecord = inputFeatureRecord_2,
              avgDepth = 2.3516559071362195,
              anomalyScore = 0.5,
              presentTreeCount = 50,
              anomaly = false,
            ),
            AnomalyDetectionResult(
              labels = List("feature_1"),
              recordKey = "3",
              inputRecord = inputFeatureRecord_3,
              avgDepth = 2.3516559071362195,
              anomalyScore = 0.5,
              presentTreeCount = 50,
              anomaly = false,
            ),
            AnomalyDetectionResult(
              labels = List("feature_1"),
              recordKey = "4",
              inputRecord = inputFeatureRecord_4,
              avgDepth = 2.3516559071362195,
              anomalyScore = 0.5,
              presentTreeCount = 50,
              anomaly = false,
            )
          )
        )))
    },

    /** a simple scenario with one feature and the almost the same values for every record (no anomalies) */
    testM("happy_path_one_feature_no_anomalies_2") {
      val inputFeatureRecord_1 = AnomalyDetectionInputFeatureRecord("1", List(1))
      val inputFeatureRecord_2 = AnomalyDetectionInputFeatureRecord("2", List(2))
      val inputFeatureRecord_3 = AnomalyDetectionInputFeatureRecord("3", List(1))
      val inputFeatureRecord_4 = AnomalyDetectionInputFeatureRecord("4", List(2))
      val layer: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = IsolationForest.live

      IsolationForest.anomalyDetection(List("feature_x"), List(inputFeatureRecord_1, inputFeatureRecord_2, inputFeatureRecord_3, inputFeatureRecord_4), 50, 50, 100, Some(1)).provideLayer(layer)
        .map(n => assert(n)(equalTo(
          List(
            AnomalyDetectionResult(
              labels = List("feature_x"),
              recordKey = "1",
              inputRecord = inputFeatureRecord_1,
              avgDepth = 2,
              anomalyScore = 0.5546061204536473,
              presentTreeCount = 50,
              anomaly = false,
            ),
            AnomalyDetectionResult(
              labels = List("feature_x"),
              recordKey = "2",
              inputRecord = inputFeatureRecord_2,
              avgDepth = 2,
              anomalyScore = 0.5546061204536473,
              presentTreeCount = 50,
              anomaly = false,
            ),
            AnomalyDetectionResult(
              labels = List("feature_x"),
              recordKey = "3",
              inputRecord = inputFeatureRecord_3,
              avgDepth = 2,
              anomalyScore = 0.5546061204536473,
              presentTreeCount = 50,
              anomaly = false,
            ),
            AnomalyDetectionResult(
              labels = List("feature_x"),
              recordKey = "4",
              inputRecord = inputFeatureRecord_4,
              avgDepth = 2,
              anomalyScore = 0.5546061204536473,
              presentTreeCount = 50,
              anomaly = false,
            )
          )
        )))
    },

    /** a simple scenario with one feature and one clear anomaly in the upper region */
    testM("happy_path_one_feature_one_upper_anomaly") {
      val inputFeatureRecord_1 = AnomalyDetectionInputFeatureRecord("1", List(1))
      val inputFeatureRecord_2 = AnomalyDetectionInputFeatureRecord("2", List(2))
      val inputFeatureRecord_3 = AnomalyDetectionInputFeatureRecord("3", List(1))
      val inputFeatureRecord_4 = AnomalyDetectionInputFeatureRecord("4", List(2))
      val inputFeatureRecord_5 = AnomalyDetectionInputFeatureRecord("5", List(2))
      val inputFeatureRecord_6 = AnomalyDetectionInputFeatureRecord("6", List(2))
      val inputFeatureRecord_7 = AnomalyDetectionInputFeatureRecord("7", List(2))
      val inputFeatureRecord_8 = AnomalyDetectionInputFeatureRecord("8", List(22))
      val inputFeatureRecord_9 = AnomalyDetectionInputFeatureRecord("9", List(2))
      val inputFeatureRecord_10 = AnomalyDetectionInputFeatureRecord("10", List(2))
      val layer: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = IsolationForest.live

      IsolationForest.anomalyDetection(List("feature_x"), List(
        inputFeatureRecord_1,
        inputFeatureRecord_2,
        inputFeatureRecord_3,
        inputFeatureRecord_4,
        inputFeatureRecord_5,
        inputFeatureRecord_6,
        inputFeatureRecord_7,
        inputFeatureRecord_8,
        inputFeatureRecord_9,
        inputFeatureRecord_10), 50, 50, 100, Some(1)).provideLayer(layer)
        .map(n => assert(n.map(_.anomaly))(equalTo(List(false, false, false, false, false, false, false, true, false, false)))
      )
    },
    /** a simple scenario with one feature and one clear anomaly in the lower region */
    testM("happy_path_one_feature_one_lower_anomaly") {
      val inputFeatureRecord_1 = AnomalyDetectionInputFeatureRecord("1", List(1))
      val inputFeatureRecord_2 = AnomalyDetectionInputFeatureRecord("2", List(20))
      val inputFeatureRecord_3 = AnomalyDetectionInputFeatureRecord("3", List(21))
      val inputFeatureRecord_4 = AnomalyDetectionInputFeatureRecord("4", List(21))
      val inputFeatureRecord_5 = AnomalyDetectionInputFeatureRecord("5", List(22))
      val inputFeatureRecord_6 = AnomalyDetectionInputFeatureRecord("6", List(23))
      val inputFeatureRecord_7 = AnomalyDetectionInputFeatureRecord("7", List(22))
      val inputFeatureRecord_8 = AnomalyDetectionInputFeatureRecord("8", List(23))
      val inputFeatureRecord_9 = AnomalyDetectionInputFeatureRecord("9", List(21))
      val inputFeatureRecord_10 = AnomalyDetectionInputFeatureRecord("10", List(24))
      val layer: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = IsolationForest.live

      IsolationForest.anomalyDetection(List("feature_x"), List(
        inputFeatureRecord_1,
        inputFeatureRecord_2,
        inputFeatureRecord_3,
        inputFeatureRecord_4,
        inputFeatureRecord_5,
        inputFeatureRecord_6,
        inputFeatureRecord_7,
        inputFeatureRecord_8,
        inputFeatureRecord_9,
        inputFeatureRecord_10), 50, 50, 100, Some(1)).provideLayer(layer)
        .map(n => assert(n.map(r => (r.anomalyScore)))(equalTo(List(
          0.8586640465796392, 0.543617107148175, 0.42986584599926375, 0.42986584599926375, 0.543617107148175, 0.543617107148175, 0.543617107148175, 0.543617107148175, 0.42986584599926375, 0.6330963888771086))))
    },
    /** a simple scenario with one feature and one clear anomaly in the middle region */
    testM("happy_path_one_feature_one_middle_anomaly") {
      val inputFeatureRecord_1 = AnomalyDetectionInputFeatureRecord("1", List(20))
      val inputFeatureRecord_2 = AnomalyDetectionInputFeatureRecord("2", List(20))
      val inputFeatureRecord_3 = AnomalyDetectionInputFeatureRecord("3", List(21))
      val inputFeatureRecord_4 = AnomalyDetectionInputFeatureRecord("4", List(22))
      val inputFeatureRecord_5 = AnomalyDetectionInputFeatureRecord("5", List(23))
      val inputFeatureRecord_6 = AnomalyDetectionInputFeatureRecord("6", List(100))
      val inputFeatureRecord_7 = AnomalyDetectionInputFeatureRecord("7", List(200))
      val inputFeatureRecord_8 = AnomalyDetectionInputFeatureRecord("8", List(201))
      val inputFeatureRecord_9 = AnomalyDetectionInputFeatureRecord("9", List(202))
      val inputFeatureRecord_10 = AnomalyDetectionInputFeatureRecord("10", List(203))
      val layer: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = IsolationForest.live

      IsolationForest.anomalyDetection(List("feature_x"), List(
        inputFeatureRecord_1,
        inputFeatureRecord_2,
        inputFeatureRecord_3,
        inputFeatureRecord_4,
        inputFeatureRecord_5,
        inputFeatureRecord_6,
        inputFeatureRecord_7,
        inputFeatureRecord_8,
        inputFeatureRecord_9,
        inputFeatureRecord_10), 50, 50, 100, Some(1)).provideLayer(layer)
        .map(n => assert(n.map(r => (r.anomalyScore)))(equalTo(List(
          0.4008110376092352, 0.4008110376092352, 0.4667844650137693, 0.543617107148175, 0.6330963888771086, 0.7373039448885209, 0.543617107148175, 0.543617107148175, 0.6330963888771086, 0.7373039448885209))))
    }
  )


}



