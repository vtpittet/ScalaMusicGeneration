package writingTest

import tonalSystem.Tone._
import utils.Print
import utils.MelodyWriter

object InterleaveImpl extends App with MelodyWriter{
  
  val segtA = I * 3
  val segtB = III * 3
  
  
  val iterB = segtB.notes.iterator
  
  val segtAB = segtA mapNotes { _ + iterB.next }
  
  Print(segtA)
  println
  Print(segtB)
  println
  Print(segtAB)
}