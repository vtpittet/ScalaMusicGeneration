package writingTest

import tonalSystem.I
import utils.Print
import segmentSystem.MusicalSegment
import utils.SS
import utils.PS
import org.scalatest.FunSuite

class FlatAllTest extends FunSuite {
  
  val i = I()
  
  test("tests correct and full recursion over SS, and keeping PS") {
    
    val s1 = SS(SS(i, i), SS(i, SS(i, i)), PS(i, PS(i, i)))
    val s1flat = SS(i, i, i, i, i, PS(i, i, i))
    assert(s1.flatAll == s1flat)
  }
  
  test("tests correct and full recursion over PS, and keeping SS") {
    val s2 = PS(PS(i, i), PS(i, PS(i, i)), SS(i, SS(i, i)))
    val s2flat = PS(i, i, i, i, i, SS(i, i, i))
    assert(s2.flatAll == s2flat)
  }
  
  test("tests correct extraction of single track in SequentialSegment") {
    val s3 = SS(SS(i), PS(i), PS(i))
    val s3flat = SS(i, i, i)
    assert(s3.flatAll == s3flat)
  }
  
  test("tests correct extraction of single track in ParallelSegment") {
    val s4 = PS(PS(i), SS(i), SS(i))
    val s4flat = PS(i, i, i)
    assert(s4.flatAll == s4flat)
  }
}