package segment

import segmentSystem.ClassPredicate.isNote
import tonalSystem.Tone._
import utils.MelodyWriter
import segmentSystem.ClassPredicate.isSeq
import org.scalatest.FunSuite
import segmentSystem.ClassPredicate.isPar


class PredicateTests extends FunSuite with MelodyWriter {

  val n: MS = I
  val s: MS = I + I
  val p: MS = I | I
  
  val nos = isNote orElse isSeq
  val nop = isNote orElse isPar
  val sop = isSeq orElse isPar
  val nosop = isNote orElse isSeq orElse isPar
  
  test("note predicate") {
    assert(isNote.isDefinedAt(n))
    assert(!isNote.isDefinedAt(s))
    assert(!isNote.isDefinedAt(p))
  }
  
  test("seq predicate") {
    assert(isSeq.isDefinedAt(s))
    assert(!isSeq.isDefinedAt(n))
    assert(!isSeq.isDefinedAt(p))
  }
  
  test("par predicate") {
    assert(isPar.isDefinedAt(p))
    assert(!isPar.isDefinedAt(n))
    assert(!isPar.isDefinedAt(s))
  }
  
  test("note orElse seq") {
    assert(nos.isDefinedAt(n))
    assert(nos.isDefinedAt(s))
    assert(!nos.isDefinedAt(p))
  }
  
  test("note orElse par") {
    assert(nop.isDefinedAt(n))
    assert(!nop.isDefinedAt(s))
    assert(nop.isDefinedAt(p))
  }
  
  test("seq orElse par") {
    assert(!sop.isDefinedAt(n))
    assert(sop.isDefinedAt(s))
    assert(sop.isDefinedAt(p))
  }
  
  test("note orElse seq orElse par") {
    assert(nosop.isDefinedAt(n))
    assert(nosop.isDefinedAt(s))
    assert(nosop.isDefinedAt(p))
  }
  
  test("note pred with bool fun") {
    val nb = isNote given {_.tone == I}
    assert(nb.isDefinedAt(n))
    assert(!nb.isDefinedAt(n+1))
  }
  
  test("seq pred with bool fun") {
    val sb = isSeq given {_.height > 1}
    val ss = I ++ I ++ I
    assert(sb.isDefinedAt(ss))
    assert(!sb.isDefinedAt(s))
  }
  
  test("combining two different class pred") {
    val sbonb = (isSeq given {_.height > 1}) orElse (isNote given {_.tone == I})
    val st = I ++ I ++ I
    val sf = I + I
    val nt: N = I
    val nf: N = II
    
    assert(sbonb.isDefinedAt(st))
    assert(!sbonb.isDefinedAt(sf))
    assert(sbonb.isDefinedAt(nt))
    assert(!sbonb.isDefinedAt(nf))
  }
}