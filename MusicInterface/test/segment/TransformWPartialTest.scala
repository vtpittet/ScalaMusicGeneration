package segment

import utils.MelodyWriter
import org.scalatest.FunSuite
import segmentSystem.SequentialSegment
import segmentSystem.MusicalSegment
import segmentSystem.ParallelSegment
import tonalSystem.Tone._
import segmentSystem.Transform
import segmentSystem.ClassPredicate.isNote
import segmentSystem.ClassPredicate.isSeq

class TransformWPartialTest extends FunSuite with MelodyWriter {
  
  test("apply to sequentialSegment only") {
    val mBefore = (I + I) || (I + I) | (I | I)
    val mAfter = mBefore ++> (isSeq thenDo (_ *2))
    val mExpected = (I + I) *2 || (I + I) *2 | (I | I)
    assert(mAfter == mExpected)
  }
  
  test("apply partial with boolean predicate") {
    val mBefore = I + I + I + II
    val mAfter = mBefore ++> (isNote given (_.tone == I) thenDo (_ + 1, 2, 1))
    val mExpected = I + II + I + II
    assert(mAfter == mExpected)
  }
}