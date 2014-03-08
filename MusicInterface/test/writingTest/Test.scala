package writingTest

import segmentSystem.SimpleMelody
import tonalSystem._
import utils.Print
import segmentSystem.Note
import rythmics.BPM
import rythmics.W
import rythmics.Q

object Test extends App {
  
  val harp = SimpleMelody((I, 1, 0), (III, 1, 0), (V, 1, 0), (VII, 1, 0))
  
  println
  println("C Major")
  Print(harp, Major(C))
  
  println
  println("C Minor")
  Print(harp, MinorNatural(C))
  
  
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
  
  
  val full2 = (newSegt *+ ((x=>x), _+2)) *| ((x=>x), _>>(Q, Q, Q, Q))
  
  println
  println("similar but not same")
  Print(full2)
  
  
}