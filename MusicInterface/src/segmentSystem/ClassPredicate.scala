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
object ClassPredicate {
  implicit class TransfListBuilder[T <: MusicalSegment](cp: ClassPredicate[T]) {
    // the following method cannot stand in ClassPredicate because T would appear in contraviariant position.
    def thenDo(f: T => MusicalSegment, period: Int = 1, from: Int = 0, to: Int = -1): TransformList[T] = {
      new TransformList(cp, Transform(f, period, from, to)::Nil)
    }
  }
}

object IsNote extends ClassPredicate[Note](_ match {case n: Note => n})
object IsSeq extends ClassPredicate[SequentialSegment](_ match {case s: SequentialSegment => s})
object IsPar extends ClassPredicate[ParallelSegment](_ match {case p: ParallelSegment => p})
object IsMus extends ClassPredicate[MusicalSegment](_ match {case m: MusicalSegment => m})