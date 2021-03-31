package services

import model.Model.{ExternalNode, ITree, InternalNode}

import scala.annotation.tailrec
import scala.util.Random

/** All the helper functions that are used in the Isolation forest algorithm */
object AlgorithmUtil {

  /** recursive function to generate an isolation tree consisting of the ADT types. Specify the max depth and count is zero when starting */
  def isolationTree(data: List[Double], counter: Int, maxDepth: Int, randomSeed: Option[Int] = None): ITree[List[Double]] = {
    if (counter >= maxDepth || data.isEmpty || data.distinct.size == 1) {
      ExternalNode(data)
    } else {
      val randomSplitElement: Double = getRandomSplit(data.min, data.max, randomSeed)
      val split: (List[Double], List[Double]) = splitList(data, randomSplitElement)
      if (split._1.isEmpty || split._2.isEmpty) {
        if (split._1.isEmpty) ExternalNode(split._1) else ExternalNode(split._2)
      } else {
        InternalNode(randomSplitElement, isolationTree(split._1, counter + 1, maxDepth, randomSeed), isolationTree(split._2, counter + 1, maxDepth, randomSeed))
      }
    }
  }

  /** Building an isolation tree */
  def buildTree(data: List[Double], maxDepth: Int, randomSeed: Option[Int] = None): ITree[List[Double]] = {
    AlgorithmUtil.isolationTree(data, 0, maxDepth, randomSeed)
  }

  /** get the path length inside an Itree for a value */
  @tailrec
  def pathLength(tree: ITree[List[Double]], inputValue: Double, counter: Double): Option[Double] = {
    tree match {
      case InternalNode(splitValue, left, right) =>
        if (inputValue <= splitValue) pathLength(left, inputValue, counter + 1)
        else pathLength(right, inputValue, counter + 1)
      case ExternalNode(value) =>
        if (value.contains(inputValue)) {
          if (value.size > 1) Some(counter + AlgorithmUtil.c(value.size)) else Some(counter)
        } else {
          None
        }
    }
  }


  /** generate a random value in a specified interval and use the seed if provided */
  def getRandomSplit(start: Double, end: Double, randomSeed: Option[Int] = None): Double = {
    randomSeed.map(new Random(_).between(start + 0.00000001, end)).getOrElse(Random.between(start + 0.00000001, end))
  }

  /** generate 2 lists from 1 list based on a split value */
  def splitList(input: List[Double], split: Double): (List[Double], List[Double]) = {
    val belowList: List[Double] = input.filter(_ <= split)
    val aboveList: List[Double] = input.filter(_ > split)
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



}
