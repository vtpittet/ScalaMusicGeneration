package segment

import segmentSystem.IsNote
import tonalSystem.I
import utils.MelodyWriter
import segmentSystem.IsSeq
import org.scalatest.FunSuite
import tonalSystem.II
import segmentSystem.IsPar


class PredicateTests extends FunSuite with MelodyWriter {

  val n: MS = I
  val s: MS = I + I
  val p: MS = I | I
  
  val nos = IsNote orElse IsSeq
  val nop = IsNote orElse IsPar
  val sop = IsSeq orElse IsPar
  val nosop = IsNote orElse IsSeq orElse IsPar
  
  test("note predicate") {
    assert(IsNote.isDefinedAt(n))
    assert(!IsNote.isDefinedAt(s))
    assert(!IsNote.isDefinedAt(p))
  }
  
  test("seq predicate") {
    assert(IsSeq.isDefinedAt(s))
    assert(!IsSeq.isDefinedAt(n))
    assert(!IsSeq.isDefinedAt(p))
  }
  
  test("par predicate") {
    assert(IsPar.isDefinedAt(p))
    assert(!IsPar.isDefinedAt(n))
    assert(!IsPar.isDefinedAt(s))
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
    val nb = IsNote given {_.tone == I}
    assert(nb.isDefinedAt(n))
    assert(!nb.isDefinedAt(n+1))
  }
  
  test("seq pred with bool fun") {
    val sb = IsSeq given {_.height > 1}
    val ss = I ++ I
    assert(sb.isDefinedAt(ss))
    assert(!sb.isDefinedAt(s))
  }
  
  test("combining two different class pred") {
    val sbonb = (IsSeq given {_.height > 1}) orElse (IsNote given {_.tone == I})
    val st = I ++ I
    val sf = I + I
    val nt: N = I
    val nf: N = II
    
    assert(sbonb.isDefinedAt(st))
    assert(!sbonb.isDefinedAt(sf))
    assert(sbonb.isDefinedAt(nt))
    assert(!sbonb.isDefinedAt(nf))
  }
}