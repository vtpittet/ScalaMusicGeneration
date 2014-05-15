package writingTest

import tonalSystem.III
import tonalSystem.I
import utils.Print
import utils.MelodyWriter

object InterleaveImpl extends App with MelodyWriter{
  
  val segtA = I * 3
  val segtB = III * 3
  
  
  val iterB = segtB.notes.iterator
  
  val segtAB = segtA +> { _ + iterB.next}
  
  Print(segtA)
  println
  Print(segtB)
  println
  Print(segtAB)
}