package segmentSystem

case class Transform(apply: PartialFunction[MusicalSegment, MusicalSegment], period: Int, from: Int, to: Int)
object Transform {
  implicit def funNote2Transform(apply: Note => MusicalSegment): Transform = {
    Transform({ case n: Note => apply(n) }, 1, 0, -1)
  }
  implicit def funNoteWithPeriod2Transform(fp: (Note => MusicalSegment, Int)): Transform = { 
    Transform({ case n: Note => fp._1(n) }, fp._2, 0, -1)
  }
  implicit def funNotePeriodFrom2Transform(fpf: (Note => MusicalSegment, Int, Int)): Transform = {
    Transform({ case n: Note => fpf._1(n) }, fpf._2, fpf._3, -1)
  }
  implicit def funNotePeriodFromTo2Transform(fpft: (Note => MusicalSegment, Int, Int, Int)): Transform = {
    Transform({ case n: Note => fpft._1(n) }, fpft._2, fpft._3, fpft._4)
  }
  implicit def funNoteToTransformBuilderWPeriod(apply: Note => MusicalSegment): (Int) => Transform = {
    Transform({ case n: Note => apply(n) }, _, 0, -1)
  }
  implicit def funNoteToTransformBuilderWPeriodWFrom(apply: Note => MusicalSegment): (Int, Int) => Transform = {
    Transform({ case n: Note => apply(n) }, _ , _, -1)
  }
  implicit def funNoteToTransformBuilderWPeriodWFromWTo(apply: Note => MusicalSegment): (Int, Int, Int) => Transform = {
    Transform({ case n: Note => apply(n) }, _ , _, _)
  }
  def identity: Transform = Transform({ case m => m }, 1, 0, -1)
}