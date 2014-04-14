package segment

import org.scalatest.FunSuite
import tonalSystem._
import utils.SS
import utils.MelodyWriter

class DepthControlledAdditionTest extends FunSuite with MelodyWriter {

  test("using + with only notes") {
    val tree = I + (II + (III + IV))
    assert(tree.depth == 1)
  }
  
  test("using + with tree of depth 2") {
    val tree1 = SS(I, SS(I)) + SS(I)
    val tree2 = SS(I) + SS(I, SS(I))
    val tree3 = SS(I, SS(I)) + SS(I, SS(I))
    assert(tree1.depth == 2)
    assert(tree1.melody.size == 3)
    assert(tree2.depth == 2)
    assert(tree2.melody.size == 3)
    assert(tree3.depth == 2)
    assert(tree3.melody.size == 4)
  }
  
  test("using ++ with tree of depth 2") {
    val tree1 = SS(I, SS(I)) ++ SS(I)
    val tree2 = SS(I) ++ SS(I, SS(I))
    val tree3 = SS(I, SS(I)) ++ SS(I, SS(I))
    assert(tree1.depth == 2)
    assert(tree1.melody.size == 3)
    assert(tree2.depth == 2)
    assert(tree2.melody.size == 3)
    assert(tree3.depth == 2)
    assert(tree3.melody.size == 4)
  }
  
  test("using +++ with tree of depth 2") {
    val tree1 = SS(I, SS(I)) +++ SS(I)
    val tree2 = SS(I) +++ SS(I, SS(I))
    val tree3 = SS(I, SS(I)) +++ SS(I, SS(I))
    assert(tree1.depth == 3)
    assert(tree1.melody.size == 2)
    assert(tree2.depth == 3)
    assert(tree2.melody.size == 2)
    assert(tree3.depth == 3)
    assert(tree3.melody.size == 2)
  }
  
  test("using ++++ with tree of depth 2") {
    val tree1 = SS(I, SS(I)) ++++ SS(I)
    val tree2 = SS(I) ++++ SS(I, SS(I))
    val tree3 = SS(I, SS(I)) ++++ SS(I, SS(I))
    assert(tree1.depth == 4)
    assert(tree1.melody.size == 1)
    assert(tree2.depth == 4)
    assert(tree2.melody.size == 1)
    assert(tree3.depth == 4)
    assert(tree3.melody.size == 1)
  }
}