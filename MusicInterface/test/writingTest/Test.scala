package writingTest

import org.scalatest.FunSuite
import tonalSystem._
import utils.Print
import segmentSystem.SimpleMelody
import segmentSystem.ParallelSegment
import segmentSystem.SequentialSegment

object Test extends App {
  
  val myMelody = ((I, 1) +: (II, 1) +: SimpleMelody) * 3
  
  val awfulMelody = ParallelSegment(List(myMelody, myMelody))
  
  
  val sm = SimpleMelody((I, 1, 0), (III, 1, 0))
  val parallel = ParallelSegment(List(sm, ParallelSegment(List(sm, sm)), sm))
  
  val sequential = SequentialSegment(List(sm, parallel, sm, sm))
  
  Print(sequential)
  
  val harp = SimpleMelody((I, 1, 0), (III, 1, 0), (V, 1, 0), (VII, 1, 0))
  
  println
  println("CS Major")
  Print(harp, Major(CS))
  
  println
  println("CS Minor")
  Print(harp, MinorNatural(CS))
}