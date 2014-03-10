package writingTest

import rythmics.BPM
import rythmics.Q
import segmentSystem.Note
import segmentSystem.Note
import segmentSystem.SimpleMelody
import tonalSystem._
import utils.Print

object WTest extends App {
  
//  val harp = SimpleMelody((I, 1, 0), (III, 1, 0), (V, 1, 0), (VII, 1, 0))
  
  val scale = I() + II() + III() + IV () + V() + VI() + VII() + I(Q, 1)
  
  println("G# Major")
  Print(scale, Major(G(0, 1)))
  
  println
  println("C Minor")
  Print(scale, Minor(C))
  
  
  println
  println("basic melody :")
  val segt0 = Note(O, BPM(1)) * 4
  
  val segt1 = I +: II +: III +: I +: SimpleMelody
  
  val segt2 = segt1 + 2
  
  val full = segt1 + segt2 | segt0 + segt1
  
  Print(full)
  
  val newOSegt = O(Q) * 4
  val newSegt = I() + II() + III() + I()
  val newFull = newSegt + (newSegt + 2) | newOSegt + newSegt
  
  println
  println("Same:")
  Print(newFull)
  
  
  
  println
  println("similar but not same")
  Print( ((I() *+ (_+1, _+2, identity(_))) *+ (_+2)) *| ((O(Q) * 4)+_) )
  
  
}