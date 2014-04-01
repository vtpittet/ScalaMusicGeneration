package playingTest

import tonalSystem.III
import tonalSystem.I
import utils.Print

object InterleaveImpl extends App {

  val segtA = I() * 3
  val segtB = III() * 3
  
  
  val iterB = segtB.notes.iterator
  
  val segtAB = segtA +> { _ + iterB.next}
  
  Print(segtA)
  println
  Print(segtB)
  println
  Print(segtAB)
}