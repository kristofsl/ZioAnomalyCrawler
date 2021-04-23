package services

import model.Model.{AnomalyDetectionInputFeatureRecord, AnomalyDetectionResult, ExternalNode, ITree, ITreeResult, InternalNode}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

/** All the helper functions for the isolation forest algorithm **/
object AlgorithmUtil {

  def buildForest(inputLabels: List[String], inputData: List[AnomalyDetectionInputFeatureRecord], maxTreeDepth: Int, sampleCount: Int, treeCount: Int, randomSeed: Option[Int]): List[ITreeResult] = {
    val lb: ListBuffer[ITreeResult] = new ListBuffer()
    for (i <- 1 to treeCount) {
      val subSample: List[AnomalyDetectionInputFeatureRecord] = Random.shuffle(inputData).take(sampleCount)
      println(s"Building tree with max tree depth ${maxTreeDepth} and subsample size ${subSample.length}")
      lb.append(ITreeResult(subSample.length, buildTree(subSample,inputLabels, maxTreeDepth, randomSeed)))
    }
    lb.toList
  }

  /** recursive function to generate an isolation tree consisting of the ADT types. Specify the max depth and count is zero when starting */
  def isolationTree(data: List[AnomalyDetectionInputFeatureRecord], inputLabels: List[String], counter: Int, maxDepth: Int, randomSeed: Option[Int] = None): ITree[List[AnomalyDetectionInputFeatureRecord]] = {
    val randomLabelIndex: Int = Random.nextInt(inputLabels.length)
    val randomLabel: String = inputLabels(randomLabelIndex)
    val relevantData:List[Double] = data.map(d => d.input(randomLabelIndex))
    if (counter >= maxDepth || relevantData.isEmpty || relevantData.distinct.size == 1) {
      ExternalNode(data, randomLabel, randomLabelIndex)
    } else {
      val randomSplitElement: Double = getRandomSplit(relevantData.min, relevantData.max, randomSeed)
      val split: (List[AnomalyDetectionInputFeatureRecord], List[AnomalyDetectionInputFeatureRecord]) = splitList(data,randomLabelIndex,randomSplitElement)
      if (split._1.isEmpty || split._2.isEmpty) {
        if (split._1.isEmpty) ExternalNode(split._1,randomLabel, randomLabelIndex) else ExternalNode(split._2,randomLabel, randomLabelIndex)
      } else {
        InternalNode(randomSplitElement, randomLabel, randomLabelIndex, isolationTree(split._1,inputLabels, counter + 1, maxDepth, randomSeed), isolationTree(split._2, inputLabels, counter + 1, maxDepth, randomSeed))
      }
    }
  }

  /** Building an isolation tree */
  def buildTree(data: List[AnomalyDetectionInputFeatureRecord], inputLabels: List[String], maxDepth: Int, randomSeed: Option[Int] = None): ITree[List[AnomalyDetectionInputFeatureRecord]] = {
    AlgorithmUtil.isolationTree(data,inputLabels, 0, maxDepth, randomSeed)
  }

  /** get the path length inside an Itree for a value */
  @tailrec
  def pathLength(tree: ITree[List[AnomalyDetectionInputFeatureRecord]], inputValue: AnomalyDetectionInputFeatureRecord, counter: Double): Double = {
    tree match {
      case InternalNode(splitValue, featureName,featureIndex, left, right) =>
        val i:Double = inputValue.input(featureIndex)
        if (i <= splitValue) pathLength(left, inputValue, counter + 1)
        else pathLength(right, inputValue, counter + 1)
      case ExternalNode(value, featureName, featureIndex) =>
        val featureData:List[Double] = value.map(_.input(featureIndex))
        val i:Double = inputValue.input(featureIndex)
        if (value.size > 1) counter + AlgorithmUtil.c(value.size) else counter
    }
  }


  /** generate a random value in a specified interval and use the seed if provided */
  def getRandomSplit(start: Double, end: Double, randomSeed: Option[Int] = None): Double = {
    randomSeed.map(new Random(_).between(start + 0.00000001, end)).getOrElse(Random.between(start + 0.00000001, end))
  }

  /** generate a random into from 0 to end interval (not inclusive) */
  def getRandomInt(end:Int, randomSeed: Option[Int] = None) = {
    randomSeed.map(new Random(_).nextInt(end)).getOrElse(Random.nextInt(end))
  }

  /** generate 2 lists from 1 list based on a split value */
  def splitList(input: List[AnomalyDetectionInputFeatureRecord], featureIndex: Int, split: Double): (List[AnomalyDetectionInputFeatureRecord], List[AnomalyDetectionInputFeatureRecord]) = {
    val belowList: List[AnomalyDetectionInputFeatureRecord] = input.filter(_.input(featureIndex) <= split)
    val aboveList: List[AnomalyDetectionInputFeatureRecord] = input.filter(_.input(featureIndex) > split)
    (belowList, aboveList)
  }

  /** c function to compensate if a leaf has multiple values */
  def c(externalNodeSize: Int): Double = {
    if (externalNodeSize > 2) {
      (2 * H(externalNodeSize - 1)) - ((2 * (externalNodeSize - 1)) / externalNodeSize)
    } else if (externalNodeSize == 2) 1 else 0
  }

  def H(input: Double): Double = {
    Math.log(input) + 0.5772156649
  }

  def getAnomalyResult(inputDataRecord: AnomalyDetectionInputFeatureRecord, forest: List[ITreeResult], inputLabels: List[String]): AnomalyDetectionResult = {
    val r:List[Double] = forest.toSeq.map(t => {
        AlgorithmUtil.pathLength(t.ITree,inputDataRecord,0)
    })

    val avgDepth:Double = r.sum / r.length
    val score:Double = if (avgDepth == 0) 0.0 else { Math.pow(2, -avgDepth / AlgorithmUtil.c(forest.head.sampleSize)) }
    val result = new AnomalyDetectionResult(
        labels = inputLabels,
        recordKey = inputDataRecord.recordKey,
        inputRecord = inputDataRecord,
        avgDepth = avgDepth,
        anomalyScore = score,
        presentTreeCount = r.size,
        anomaly = score >= 0.70
      )
    println(s"Anomaly result ${result}")
    result
    }
}
