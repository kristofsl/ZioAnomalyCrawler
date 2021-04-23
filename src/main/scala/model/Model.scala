package model

import java.time.LocalDateTime

object Model {

  /** ADT for the Isolation tree */
  sealed trait ITree[+A]
  case class ExternalNode[A](value: A, featureName: String, featureIndex: Int) extends ITree[A]
  case class InternalNode[A](splitValue:Double, featureName: String, featureIndex: Int, left: ITree[A], right: ITree[A]) extends ITree[A]

  /** case classes for the isolation forest algorithm */
  case class AnomalyDetectionResult(labels: List[String], recordKey: String, inputRecord: AnomalyDetectionInputFeatureRecord, avgDepth:Double, anomalyScore:Double, presentTreeCount: Int, anomaly:Boolean)
  case class AnomalyDetectionInputFeatureRecord(recordKey: String, input: List[Double])
  case class IForest(trees: List[ITreeResult])
  case class ITreeResult(sampleSize: Int, ITree: ITree[List[AnomalyDetectionInputFeatureRecord]])

  /** error messages */
  case class LabelInputError(message: String) extends Exception {
    override def getMessage: String = message
  }
  case class LabelMismatchError(message: String) extends Exception {
    override def getMessage: String = message
  }
  case class InvalidTreeParamsError(message: String) extends Exception {
    override def getMessage: String = message
  }

  val kafkaBatchSize = "BATCH_SIZE"
  val kafkaBatchWindowLimit = "BATCH_SECONDS_LIMIT"
}
