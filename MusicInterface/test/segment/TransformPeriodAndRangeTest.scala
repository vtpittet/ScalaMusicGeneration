package segment

import utils.MelodyWriter
import org.scalatest.FunSuite
import tonalSystem.Tone._
import segmentSystem.Note
import segmentSystem.MusicalSegment
import midiInterface.MelodyPlayer
import segmentSystem.ClassPredicate.isNote

class TransformPeriodAndRangeTest extends FunSuite with MelodyWriter {

  
  test("expand with no function (identity)") {
    val mBefore = I *3
    val mAfter = mBefore mapIf ()
    assert(mBefore == mAfter) // work because of case classes
  }
  
  test("expand with sigle funtion applied all the time") {
    val mBefore = I *3
    val mAfter = mBefore mapIf (isNote thenDo (_ +1))
    val mExpected = II *3
    assert(mAfter == mExpected)
  }
  
  test("expand with single function and period = 2") {
    val mBefore = I *3
    val mAfter = mBefore mapIf (isNote thenDo (_ +1, 2))
    val mExpected = II + I + II
    assert(mAfter == mExpected)
  }
  
  test("expand with functions interleaving") {
    val mBefore = I *4
    val mAfter = mBefore mapIf (isNote thenDo (_ + 1, 2, 0, -1) or (_ + 2, 2, 1, -1))
    val mExpected = ((II + III) *2).flatAll
    assert(mAfter == mExpected)
  }
  
  test("expand with priority erasure") {
    val mBefore = I *4
    val mAfter = mBefore mapIf (isNote thenDo (_ +1, 2) or (_ + 2))
    val mExpected = ((II + III) *2).flatAll
    assert(mAfter == mExpected)
  }
  
  test("some more complex test") {
    val mBefore = I *12
    val mAfter = mBefore mapIf (isNote thenDo (_ + 4, 4, 3) or (_ + 3, 3, 5) or (_ + 2, 2, 0, 10) or (_ + 1, 1, 1, 10))
    val mExpected = III + II + III + V + III + IV + III +  V + IV + II + I + V
    assert(mAfter == mExpected)
  }
}