package services

import model.Model._
import zio.{Has, Task, UIO, ZIO, ZLayer}

object IsolationForest {

  type IsolationForestEnv = Has[IsolationForest.Service]

  /** Utility functions for the algorithm */

  /** the specification of the service */
  trait Service {
    def anomalyDetection(inputLabels: List[String], inputData: List[AnomalyDetectionInputFeatureRecord], treeCount: Int, maxTreeDepth: Int, sampleCount: Int, randomSeed: Option[Int] = None): Task[List[AnomalyDetectionResult]]
  }

  /** the live implementation */
  val live: ZLayer[Any, Nothing, Has[IsolationForest.Service]] = ZLayer.succeed(
    new Service {
      override def anomalyDetection(inputLabels: List[String], inputData: List[AnomalyDetectionInputFeatureRecord], treeCount: Int, maxTreeDepth: Int, sampleCount: Int, randomSeed: Option[Int]): Task[List[AnomalyDetectionResult]] =
        if (inputLabels.isEmpty || inputLabels.length != inputLabels.distinct.length) {
          ZIO.fail(LabelInputError("Input labels are empty or there are duplicates in the provided input labels (labels must be unique)"))
        } else if (inputData.map(_.input.length).exists(_ != inputLabels.length)) {
          ZIO.fail(LabelMismatchError("Input labels are not matching the input data"))
        } else if (treeCount <= 1 || maxTreeDepth <= 1 || sampleCount <= 1) {
          ZIO.fail(InvalidTreeParamsError("Tree count, max tree depth or sample count should be at least 2"))
        } else {
          for {
            forest <- ZIO.effect(AlgorithmUtil.buildForest(inputLabels, inputData, maxTreeDepth, sampleCount, treeCount, randomSeed))
            anomalyDetectionResultList: List[AnomalyDetectionResult] <- ZIO.foreachPar(inputData) {
              (inputData: AnomalyDetectionInputFeatureRecord) => {
                ZIO.effect(AlgorithmUtil.getAnomalyResult(inputData, forest, inputLabels))
              }
            }
          } yield anomalyDetectionResultList
        }
    }
  )

  /** a clean high level method for accessing the service */
  def anomalyDetection(inputLabels: List[String], inputData: List[AnomalyDetectionInputFeatureRecord], treeCount: Int, maxTreeDepth: Int, sampleCount: Int, randomSeed: Option[Int] = None): ZIO[Has[IsolationForest.Service], Throwable, List[AnomalyDetectionResult]] = {
    ZIO.accessM(hasService => hasService.get.anomalyDetection(inputLabels, inputData, treeCount, maxTreeDepth, sampleCount, randomSeed))
  }

}
