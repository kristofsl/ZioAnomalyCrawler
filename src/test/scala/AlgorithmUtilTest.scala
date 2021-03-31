import model.Model.{ExternalNode, ITree, InternalNode}
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite
import services.AlgorithmUtil
import services.AlgorithmUtil.pathLength

class AlgorithmUtilTest extends AnyFunSuite with BeforeAndAfter {

  before(
    // Setup
  )

  test(testName = "build_tree_simple") {
    val t: ITree[List[Double]] = AlgorithmUtil.buildTree(List(1,2,3,4,5,6,7), maxDepth = 100, randomSeed = Some(1))
    assert(t.toString == "InternalNode(5.3852691469109635,InternalNode(3.9235127655043813,InternalNode(2.4617563840978,InternalNode(1.730878193394509,ExternalNode(List(1.0)),ExternalNode(List(2.0))),ExternalNode(List(3.0))),InternalNode(4.730878193394509,ExternalNode(List(4.0)),ExternalNode(List(5.0)))),InternalNode(6.730878193394509,ExternalNode(List(6.0)),ExternalNode(List(7.0))))")
  }

  test(testName = "build_tree_simple_duplicates") {
    val t: ITree[List[Double]] = AlgorithmUtil.buildTree(List(1,2,3,4,4,4,5,5,6,7), maxDepth = 100, randomSeed = Some(1))
    assert(t.toString == "InternalNode(5.3852691469109635,InternalNode(3.9235127655043813,InternalNode(2.4617563840978,InternalNode(1.730878193394509,ExternalNode(List(1.0)),ExternalNode(List(2.0))),ExternalNode(List(3.0))),InternalNode(4.730878193394509,ExternalNode(List(4.0, 4.0, 4.0)),ExternalNode(List(5.0, 5.0)))),InternalNode(6.730878193394509,ExternalNode(List(6.0)),ExternalNode(List(7.0))))")
  }

  test(testName = "build_tree_simple_depth_limit") {
    val t: ITree[List[Double]] = AlgorithmUtil.buildTree(List(1,2,3,4,4,4,5,5,6,7), maxDepth = 2, randomSeed = Some(1))
    assert(t.toString == "InternalNode(5.3852691469109635,InternalNode(3.9235127655043813,ExternalNode(List(1.0, 2.0, 3.0)),ExternalNode(List(4.0, 4.0, 4.0, 5.0, 5.0))),InternalNode(6.730878193394509,ExternalNode(List(6.0)),ExternalNode(List(7.0))))")
  }

  test(testName = "path_length") {
    // simple tree cases
    assert(AlgorithmUtil.pathLength(ExternalNode(List(1)), 1, 0) == Some(0))
    assert(AlgorithmUtil.pathLength(ExternalNode(List(1)), 2, 0) == None)
    assert(AlgorithmUtil.pathLength(tree = InternalNode(splitValue = 1, left = ExternalNode(List(0.4)), right = ExternalNode(List(3))), inputValue = 0.4, 0) == Some(1.0))
    assert(AlgorithmUtil.pathLength(tree = InternalNode(splitValue = 1, left = ExternalNode(List(0.4)), right = ExternalNode(List(3))), inputValue = 3, 0) == Some(1.0))

    // invalid tree case, but we expect to get the length of the last leaf
    assert(AlgorithmUtil.pathLength(tree = InternalNode(splitValue = 1, left = ExternalNode(List(0.4)), right = ExternalNode(List(3))), inputValue = 5, 0) == None)
    assert(AlgorithmUtil.pathLength(tree = InternalNode(splitValue = 1, left = ExternalNode(List(0.4)), right = ExternalNode(List(3))), inputValue = 0.4, 0) == Some(1))

    // deeper tree cases
    assert(AlgorithmUtil.pathLength(
      tree = InternalNode(
        splitValue = 100,
        left = InternalNode(
          splitValue = 50,
          left = ExternalNode(List(20)),
          right = ExternalNode(List(60))),
        right = ExternalNode(List(101)))
      , inputValue = 60, counter = 0) == Some(2))

    assert(AlgorithmUtil.pathLength(
      tree = InternalNode(
        splitValue = 100,
        left = InternalNode(
          splitValue = 50,
          left = ExternalNode(List(20)),
          right = ExternalNode(List(60))),
        right = ExternalNode(List(101)))
      , inputValue = 60, counter = 2) == Some(4))

    assert(AlgorithmUtil.pathLength(
      tree = InternalNode(
        splitValue = 100,
        left = InternalNode(
          splitValue = 50,
          left = ExternalNode(List(20)),
          right = ExternalNode(List(60))),
        right = ExternalNode(List(101)))
      , inputValue = 20, counter = 0) == Some(2))

    assert(AlgorithmUtil.pathLength(
      tree = InternalNode(
        splitValue = 100,
        left = InternalNode(
          splitValue = 50,
          left = ExternalNode(List(20)),
          right = ExternalNode(List(60))),
        right = ExternalNode(List(101)))
      , inputValue = 101, counter = 0) == Some(1))
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
    val r = AlgorithmUtil.splitList(List(1, 2, 3, 4), 2.5)
    assert(!r._1.zip(List(1, 2)).exists(x => x._1 != x._2))
    assert(!r._2.zip(List(3, 4)).exists(x => x._1 != x._2))

    val r2 = AlgorithmUtil.splitList(List(1, 2, 3, 4), 3)
    assert(r2._1.zip(List(1, 2, 3)).count(x => x._1 != x._2) == 0)
    assert(!r2._2.zip(List(4)).exists(x => x._1 != x._2))

    val r3 = AlgorithmUtil.splitList(List(1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 5), 4.00001)
    assert(!r3._1.zip(List(1, 2, 3, 4, 4, 4, 4, 4, 4, 4)).exists(x => x._1 != x._2))
    assert(r3._2.zip(List(5)).count(x => x._1 != x._2) == 0)

    val r4 = AlgorithmUtil.splitList(List(1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 5), 5.5)
    assert(!r4._1.zip(List(1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 5)).exists(x => x._1 != x._2))
    assert(r4._2.isEmpty)
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
