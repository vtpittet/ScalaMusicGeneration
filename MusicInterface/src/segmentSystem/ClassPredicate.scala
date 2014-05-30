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

  object isNote extends ClassPredicate[Note](_ match {case n: Note => n})
  object isSeq extends ClassPredicate[Sequential](_ match {case s: Sequential => s})
  object isPar extends ClassPredicate[Parallel](_ match {case p: Parallel => p})
  object isMus extends ClassPredicate[MusicalSegment](_ match {case m: MusicalSegment => m})
}