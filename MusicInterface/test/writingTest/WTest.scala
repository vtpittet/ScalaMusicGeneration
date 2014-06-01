package writingTest

import rythmics.{E => rE}
import segmentSystem.Note
import segmentSystem.Note
import tonalSystem._
import tonalSystem.Tone._
import utils.MelodyWriter
import utils.Print
import segmentSystem.MusicalSegment
import segmentSystem.SequentialSegment
import segmentSystem.Transform
import segmentSystem.ClassPredicate.isSeq
import segmentSystem.ClassPredicate.isNote

object WTest extends App with MelodyWriter {
  
//  val harp = SimpleMelody((I, 1, 0), (III, 1, 0), (V, 1, 0), (VII, 1, 0))
  
  val scale = I + II + III + IV + V + VI + VII + I(1)
  
  println("G# Major")
  Print(scale, Major(G(0, 1)))
  
  println
  println("C Minor")
  Print(scale, Minor(C))
  
  /*
  println
  println("basic melody :")
  val segt0 = Note(O, BPM(1)) * 4
  
  val segt1 = I +: II +: III +: I +: SimpleMelody
  
  val segt2 = segt1 + 2
  
  val full = segt1 + segt2 | segt0 + segt1
  
  Print(full)
  val newOSegt = O(Q) * 4
  val newSegt = I + II + III + I
  val newFull = newSegt + (newSegt + 2) | newOSegt + newSegt
  
  println
  println("Same:")
  Print(newFull)
  
  
  
  println
  println("similar but not same")
  Print( ((I *+ (_+1, _+2, identity(_))) *+ (_+2)) *| ((O(Q) * 4)+_) )
  
  */
  val m = (I + I) || (I + I) | (I | I)
  
  def f(m: MS): MS = m mapIf (isSeq given (_.height == 1) thenDo (_ *2))
  println(f(m))
}