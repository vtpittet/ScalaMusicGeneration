package segmentSystem

case class Transform[T](period: Int, phaseShift: Int, apply: T => MusicalSegment)
object Transform {
  implicit def fun2Transform[T](apply: T => MusicalSegment): Transform[T] = Transform(1, 1, apply)
  implicit def periodWithFun2Transform[T](pf: (Int, T => MusicalSegment)): Transform[T] = 
        Transform(pf._1, 1, pf._2)
  implicit def periodPhaseShiftFun2Transform[T](ppf: (Int, Int, T => MusicalSegment)): Transform[T] =
        Transform(ppf._1, ppf._2, ppf._3)
}