package segment

import utils.MelodyWriter
import org.scalatest.FunSuite
import tonalSystem.I
import segmentSystem.SequentialSegment
import segmentSystem.MusicalSegment
import segmentSystem.ParallelSegment

class TransformWPartialTest extends FunSuite with MelodyWriter {
  
  test("apply to sequentialSegment only") {
    val mBefore = (I + I) || (I + I) || (I | I)
    val mAfter = mBefore ++> (
      (x: SS) => x *2
    )
    val mExpected = (I + I) *2 | (I + I) *2 | (I | I)
    assert(mAfter == mExpected)
  }
  
  test("apply alternatively to Sequential and parallel") {
    val mBefore1 = (I + I) || (I | I)
    val mBefore2 = (I | I) || (I + I)
    def after(m: MS): MS = m ++> (
      ((x: SS) => x *2)(2),
      ((x: PS) => x *3)(2, 1)
    )
    val mExpected1 = (I + I) *2 || (I | I) *3
    val mExpected2 = (I | I) || (I + I) *2
    
    assert(mExpected1 == after(mBefore1))
    assert(mExpected2 == after(mBefore2))
  }
  
  test("apply partial with boolean predicate") {
  }
}