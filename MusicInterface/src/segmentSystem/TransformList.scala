package segmentSystem

import segmentSystem.ClassPredicate.isMus

class TransformList[T <: MusicalSegment](selector: ClassPredicate[T], transfs: List[Transform[T]] = Nil) {
  
  type TStream = Stream[PartialFunction[MusicalSegment, MusicalSegment]]
  
  def orDo(apply: T => MusicalSegment, period: Int = 1, from: Int = 0, to: Int = -1): TransformList[T] = {
    new TransformList(selector, transfs ::: Transform(apply, period, from, to) :: Nil)
  }
  
  def toStream: TStream = {
    def tStream(counter: Int = 0): TStream = {
      (transfs filter { t =>
        counter >= t.from &&
        (counter < t.to || t.to == -1) &&
        (counter-t.from) % t.period == 0
      } match {
        case Nil => selector andThen identity
        case t :: ts => selector andThen t.apply
      }) #:: tStream(counter + 1)
    }
    tStream(0)
  }
}

object TransformList {
  implicit def classPredicate2TransfList[T <: MusicalSegment](cp: ClassPredicate[T]): TransformList[T] = {
    new TransformList(cp, Nil)
  }
  def identity: TransformList[MusicalSegment] = isMus thenDo (x => x)
}

case class Transform[T <: MusicalSegment](apply: T => MusicalSegment, period: Int, from: Int, to: Int)