package segmentSystem

case class Transform(apply: Note => MusicalSegment, period: Int, from: Int, to: Int)
object Transform {
  implicit def fun2Transform(apply: Note => MusicalSegment): Transform = Transform(apply, 1, 0, -1)
  implicit def funWithPeriod2Transform(fp: (Note => MusicalSegment, Int)): Transform = 
        Transform(fp._1, fp._2, 0, -1)
  implicit def funPeriodFrom2Transform(fpf: (Note => MusicalSegment, Int, Int)): Transform =
        Transform(fpf._1, fpf._2, fpf._3, -1)
  implicit def funPeriodFromTo2Transform(fpft: (Note => MusicalSegment, Int, Int, Int)): Transform =
        Transform(fpft._1, fpft._2, fpft._3, fpft._4)
  implicit def funToTransformBuilderWPeriod(apply: Note => MusicalSegment): (Int) => Transform = 
    Transform(apply, _, 0, -1)
  implicit def funToTransformBuilderWPeriodWFrom(apply: Note => MusicalSegment): (Int, Int) => Transform = {
    Transform(apply, _ , _, -1)
  }
  implicit def funToTransformBuilderWPeriodWFromWTo(apply: Note => MusicalSegment): (Int, Int, Int) => Transform = {
    Transform(apply, _ , _, _)
  }
  def identity: Transform = Transform((x: Note)=> x, 1, 0, -1)
}