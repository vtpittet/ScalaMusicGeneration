package segmentSystem

case class Transform[T <: MusicalSegment](apply: T => MusicalSegment, period: Int, from: Int, to: Int)
object Transform {
  implicit def fun2Transform[T <: MusicalSegment](apply: T => MusicalSegment): Transform[T] = Transform(apply, 1, 0, -1)
  implicit def funWithPeriod2Transform[T <: MusicalSegment](fp: (T => MusicalSegment, Int)): Transform[T] = 
        Transform(fp._1, fp._2, 0, -1)
  implicit def funPeriodFrom2Transform[T <: MusicalSegment](fpf: (T => MusicalSegment, Int, Int)): Transform[T] =
        Transform(fpf._1, fpf._2, fpf._3, -1)
  implicit def funPeriodFromTo2Transform[T <: MusicalSegment](fpft: (T => MusicalSegment, Int, Int, Int)): Transform[T] =
        Transform(fpft._1, fpft._2, fpft._3, fpft._4)
  def identity[T <: MusicalSegment]: Transform[T] = Transform[T]((x: T)=> x, 1, 0, -1)
}