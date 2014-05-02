package segment

import utils.MelodyWriter
import org.scalatest.FunSuite
import tonalSystem._
import segmentSystem.Note
import segmentSystem.MusicalSegment
import midiInterface.MelodyPlayer

class TransformMechanismsInExpandFunctionTest extends FunSuite with MelodyWriter {

  
  test("expand with no function (identity)") {
    val mBefore = I *3
    val mAfter = mBefore ++> ()
    assert(mBefore == mAfter) // work because of case classes
  }
  
  test("expand with sigle funtion applied all the time") {
    val mBefore = I *3
    val mAfter = mBefore ++> ((n: Note) => n + 1)
    val mExpected = II *3
    assert(mAfter == mExpected)
  }
  
  test("expand with functions interleaving") {
    val mBefore = I *4
    val mAfter = mBefore ++> (((n: Note) => n +1)(2, 0, -1), ((n: Note) => n + 2)(2, 1, -1))
    val mExpected = ((II + III) *2).flatAll
    assert(mAfter == mExpected)
  }
  
  test("expand with priority erasure") {
    val mBefore = I *4
    val mAfter = mBefore.++>(
        ((n: Note) => n + 1)(2),
        (n: Note) => n + 2
        )
    val mExpected = ((II + III) *2).flatAll
    assert(mAfter == mExpected)
  }
  
  test("some more complex test") {
    val mBefore = I *12
    val mAfter = mBefore ++> (
        ((n: Note) => n + 4)(4, 3),
        ((n: Note) => n + 3)(3, 5),
        ((n: Note) => n + 2)(2, 0, 10),
        ((n: Note) => n + 1)(1, 1, 10)
        )
   val mExpected = III + II + III + V + III + IV + III +  V + IV + II + I + V
   assert(mAfter == mExpected)
  }
}