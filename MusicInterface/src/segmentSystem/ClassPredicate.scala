package segmentSystem

class ClassPredicate[+ T <: MusicalSegment](cPred: PartialFunction[MusicalSegment, T])
      extends PartialFunction[MusicalSegment, T] { outer =>
  def apply(ms: MusicalSegment): T = cPred.apply(ms)
  
  def isDefinedAt(ms: MusicalSegment): Boolean = cPred.isDefinedAt(ms)
  
  def given(boolPred: T => Boolean): ClassPredicate[T] = new ClassPredicate(cPred) {
    override def isDefinedAt(ms: MusicalSegment): Boolean = {
      outer.isDefinedAt(ms) && boolPred(outer.apply(ms))
    }
  }
  
  def orElse[T1 >: T <: MusicalSegment](that: ClassPredicate[T1]): ClassPredicate[T1] = {
    new ClassPredicate(super.orElse(that))
  }
}

object NoteP extends ClassPredicate[Note](_ match {case n: Note => n})
object SeqP extends ClassPredicate[SequentialSegment](_ match {case s: SequentialSegment => s})
object ParP extends ClassPredicate[ParallelSegment](_ match {case p: ParallelSegment => p})
object MusP extends ClassPredicate[MusicalSegment](_ match {case m: MusicalSegment => m})