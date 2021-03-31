package services

import model.Model._
import services.AlgorithmUtil.buildTree
import zio.{Has, Task, UIO, ZIO, ZLayer}
import scala.collection.mutable.ListBuffer
import scala.util.Random

object IsolationForest {

  type IsolationForestEnv = Has[IsolationForest.Service]

  /** Utility functions for the algorithm */

  /** get the anomaly score results for each input record */
  def getAnomalyResult(inputDataRecord: AnomalyDetectionInputFeatureRecord, forest: List[ITreeResult], inputLabels: List[String]): Task[AnomalyDetectionResult] = {
    for {
      depthCollection: List[Option[Double]] <- ZIO.foreachPar(forest) {
        (r: ITreeResult) => {
          for {
            label <- UIO.effectTotal(r.label)
            index <- UIO.effectTotal(inputLabels.indexOf(label))
            value <- UIO.effectTotal(inputDataRecord.input(index))
            depth <- Task.effect(AlgorithmUtil.pathLength(r.ITree, value, 0))
          } yield depth
        }
      }
      filteredDepthCollection: List[Double] <- UIO.effectTotal(depthCollection.flatten)
      avgDepth <- Task.effect(filteredDepthCollection.sum / filteredDepthCollection.length)
      score <- Task.effect{ if (avgDepth == 0) 0.0 else { Math.pow(2, -avgDepth / AlgorithmUtil.c(forest.head.sampleSize)) } }
      result <- Task.effect {
        new AnomalyDetectionResult(
          labels = inputLabels,
          recordKey = inputDataRecord.recordKey,
          inputRecord = inputDataRecord,
          avgDepth = avgDepth,
          anomalyScore = score,
          presentTreeCount = filteredDepthCollection.size,
          anomaly = score >= 0.75
        )
      }

    } yield result
  }

  /** Building the forest and wrapping it in a ZIO task * */
  def buildForest(inputLabels: List[String], inputData: List[AnomalyDetectionInputFeatureRecord], maxTreeDepth: Int, sampleCount: Int, treeCount: Int, randomSeed: Option[Int]): Task[List[ITreeResult]] = {
    Task.effect {
      val lb: ListBuffer[ITreeResult] = new ListBuffer()
      for (i <- 1 to treeCount) {
        val randomLabelIndex: Int = Random.nextInt(inputLabels.length)
        val randomLabel: String = inputLabels(randomLabelIndex)
        val input: List[Double] = inputData.map(_.input(randomLabelIndex))
        val subSample: List[Double] = Random.shuffle(input).take(sampleCount)
        lb.append(ITreeResult(randomLabel, subSample.length, buildTree(subSample, maxTreeDepth, randomSeed)))
      }
      lb.toList
    }
  }

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
            forest <- buildForest(inputLabels, inputData, maxTreeDepth, sampleCount, treeCount, randomSeed)
            anomalyDetectionResultList: List[AnomalyDetectionResult] <- ZIO.foreach(inputData) {
              (inputData: AnomalyDetectionInputFeatureRecord) => {
                getAnomalyResult(inputData, forest, inputLabels)
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
