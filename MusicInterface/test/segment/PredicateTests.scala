package segment

import org.scalatest.FunSuite
import tonalSystem.I
import utils.MelodyWriter
import segmentSystem.NoteP
import segmentSystem.SeqP
import segmentSystem.ParP
import segmentSystem.ClassPredicate
import tonalSystem.II

class PredicateTests extends FunSuite with MelodyWriter {

  val n: MS = I
  val s: MS = I + I
  val p: MS = I | I
  
  val nos = NoteP orElse SeqP
  val nop = NoteP orElse ParP
  val sop = SeqP orElse ParP
  val nosop = NoteP orElse SeqP orElse ParP
  
  test("note predicate") {
    assert(NoteP.isDefinedAt(n))
    assert(!NoteP.isDefinedAt(s))
    assert(!NoteP.isDefinedAt(p))
  }
  
  test("seq predicate") {
    assert(SeqP.isDefinedAt(s))
    assert(!SeqP.isDefinedAt(n))
    assert(!SeqP.isDefinedAt(p))
  }
  
  test("par predicate") {
    assert(ParP.isDefinedAt(p))
    assert(!ParP.isDefinedAt(n))
    assert(!ParP.isDefinedAt(s))
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
    val nb = NoteP given {_.tone == I}
    assert(nb.isDefinedAt(n))
    assert(!nb.isDefinedAt(n+1))
  }
  
  test("seq pred with bool fun") {
    val sb = SeqP given {_.depth > 1}
    val ss = I ++ I
    assert(sb.isDefinedAt(ss))
    assert(!sb.isDefinedAt(s))
  }
  
  test("combining two different class pred") {
    val sbonb = (SeqP given {_.depth > 1}) orElse (NoteP given {_.tone == I})
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