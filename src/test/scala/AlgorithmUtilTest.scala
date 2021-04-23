import model.Model.{AnomalyDetectionInputFeatureRecord, ITree}
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite
import services.AlgorithmUtil
import services.AlgorithmUtil.pathLength
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.util.Random

class AlgorithmUtilTest extends AnyFunSuite with BeforeAndAfter {

  before(
    // Setup
  )

  def generateAnomalyDetectionInputOneFeatureRecord(start: Int, end: Int) = {
    AnomalyDetectionInputFeatureRecord(UUID.randomUUID().toString, List(Random.between(start + 0.00000001, end)))
  }

  test(testName = "build_tree_simple") {
    println("\nSimple test")
    var buffer: ListBuffer[AnomalyDetectionInputFeatureRecord] = new ListBuffer[AnomalyDetectionInputFeatureRecord]
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-200)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(300)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(400)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(600)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(800)))
    val t: ITree[List[AnomalyDetectionInputFeatureRecord]] = AlgorithmUtil.buildTree(buffer.toList, List("X"), maxDepth = 100, randomSeed = Some(1))
    assert(t.toString == "InternalNode(530.8781907059821,X,0,InternalNode(238.52691442466573,X,0,ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(-200.0))),X,0),InternalNode(373.0878190730203,X,0,ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(300.0))),X,0),ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(400.0))),X,0))),InternalNode(746.1756381433494,X,0,ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(600.0))),X,0),ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(800.0))),X,0)))")
  }

  test(testName = "build_tree_simple_duplicates") {
    println("\nDuplicates limit test")
    var buffer: ListBuffer[AnomalyDetectionInputFeatureRecord] = new ListBuffer[AnomalyDetectionInputFeatureRecord]
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-200)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-300)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-200)))
    val t: ITree[List[AnomalyDetectionInputFeatureRecord]] = AlgorithmUtil.buildTree(buffer.toList, List("X"), maxDepth = 100, randomSeed = Some(1))
    assert(t.toString == "InternalNode(-226.91218092697972,X,0,ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(-300.0))),X,0),ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(-200.0)), AnomalyDetectionInputFeatureRecord(X,List(-200.0))),X,0))")
  }

  test(testName = "build_tree_simple_depth_limit") {
    println("\nDepth limit test")
    var buffer: ListBuffer[AnomalyDetectionInputFeatureRecord] = new ListBuffer[AnomalyDetectionInputFeatureRecord]
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-220)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-320)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-220)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(100)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(700)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(120)))

    val t: ITree[List[AnomalyDetectionInputFeatureRecord]] = AlgorithmUtil.buildTree(buffer.toList, List("X"), maxDepth = 2, randomSeed = Some(1))
    assert(t.toString == "InternalNode(425.49575452004785,X,0,InternalNode(1.5864039121391897,X,0,ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(-220.0)), AnomalyDetectionInputFeatureRecord(X,List(-320.0)), AnomalyDetectionInputFeatureRecord(X,List(-220.0))),X,0),ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(100.0)), AnomalyDetectionInputFeatureRecord(X,List(120.0))),X,0)),ExternalNode(List(AnomalyDetectionInputFeatureRecord(X,List(700.0))),X,0))")
  }

  test(testName = "path_length") {
    println("\nPath length test")
    var buffer: ListBuffer[AnomalyDetectionInputFeatureRecord] = new ListBuffer[AnomalyDetectionInputFeatureRecord]
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-220)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-320)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-220)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(100)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(700)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(120)))
    val t: ITree[List[AnomalyDetectionInputFeatureRecord]] = AlgorithmUtil.buildTree(buffer.toList, List("X"), maxDepth = 100, randomSeed = Some(1))
    assert(pathLength(t, AnomalyDetectionInputFeatureRecord("X", List(-220)), 0) == 4.0)
    assert(pathLength(t, AnomalyDetectionInputFeatureRecord("X", List(700)), 0) == 1.0)
    assert(pathLength(t, AnomalyDetectionInputFeatureRecord("X", List(100)), 0) == 3.0)
    assert(pathLength(t, AnomalyDetectionInputFeatureRecord("X", List(22200)), 0) == 1.0)
  }

  test("random_split_no_seed") {
    assert(AlgorithmUtil.getRandomSplit(5, 10) > 5)
    assert(AlgorithmUtil.getRandomSplit(5, 10) < 10)
  }

  test("random_split_seed") {
    assert(AlgorithmUtil.getRandomSplit(5, 10, Some(1)) > 5)
    assert(AlgorithmUtil.getRandomSplit(5, 10, Some(1)) < 10)
  }

  test("split_lists") {
    var buffer: ListBuffer[AnomalyDetectionInputFeatureRecord] = new ListBuffer[AnomalyDetectionInputFeatureRecord]
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-220)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-320)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(-220)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(100)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(700)))
    buffer.append(AnomalyDetectionInputFeatureRecord("X", List(120)))
    val split = AlgorithmUtil.splitList(buffer.toList,0,150)
    assert(split._1.length == 5)
    assert(split._2.length == 1)
  }

  test("c") {
    assert(AlgorithmUtil.c(0) == 0)
    assert(AlgorithmUtil.c(1) == 0)
    assert(AlgorithmUtil.c(2) == 1)
    assert(AlgorithmUtil.c(4) == 2.3516559071362195)
    assert(AlgorithmUtil.c(10) == 4.5488804844724395)
    assert(AlgorithmUtil.c(20) == 6.04330928813288)
    assert(AlgorithmUtil.c(30) == 6.889022989772949)
    assert(AlgorithmUtil.c(1000) == 13.967940887097107)
    assert(AlgorithmUtil.c(2000) == 15.3552359988008)
  }

  after(
    // Aftercare
  )
}
