package segmentSystem

case class Transform(apply: PartialFunction[MusicalSegment, MusicalSegment], period: Int, from: Int, to: Int)
object Transform {
  type Fun[T] = T => MusicalSegment
  type Par = PartialFunction[MusicalSegment, MusicalSegment]
  
  implicit def partial2Trans(apply: Par): Transform = {
    Transform(apply, 1, 0, -1)
  }
  implicit def partial2TransBuilderWPeriod(apply: Par): Int => Transform = {
    Transform(apply, _, 0, -1)
  }
  implicit def partial2TransBuilderWPeriodWFrom(apply: Par): (Int, Int) => Transform = {
    Transform(apply, _, _, -1)
  }
  implicit def partial2TransBuilderWPeriodWFromWTo(apply: Par): (Int, Int, Int) => Transform = {
    Transform(apply, _, _, _)
  }
  
  
  implicit def funNote2Transform(apply: Note => MusicalSegment): Transform = {
    Transform({ case n: Note => apply(n) }, 1, 0, -1)
  }
  implicit def funNoteToTransformBuilderWPeriod(apply: Note => MusicalSegment): Int => Transform = {
    Transform({ case n: Note => apply(n) }, _, 0, -1)
  }
  implicit def funNoteToTransformBuilderWPeriodWFrom(apply: Note => MusicalSegment): (Int, Int) => Transform = {
    Transform({ case n: Note => apply(n) }, _ , _, -1)
  }
  implicit def funNoteToTransformBuilderWPeriodWFromWTo(apply: Note => MusicalSegment): (Int, Int, Int) => Transform = {
    Transform({ case n: Note => apply(n) }, _ , _, _)
  }
  
  
  implicit def funSeq2Transform(apply: SequentialSegment => MusicalSegment): Transform = {
    Transform({ case n: SequentialSegment => apply(n) }, 1, 0, -1)
  }
  implicit def funSeqToTransformBuilderWPeriod(apply: SequentialSegment => MusicalSegment): Int => Transform = {
    Transform({ case n: SequentialSegment => apply(n) }, _, 0, -1)
  }
  implicit def funSeqToTransformBuilderWPeriodWFrom(apply: SequentialSegment => MusicalSegment): (Int, Int) => Transform = {
    Transform({ case n: SequentialSegment => apply(n) }, _ , _, -1)
  }
  implicit def funSeqToTransformBuilderWPeriodWFromWTo(apply: SequentialSegment => MusicalSegment): (Int, Int, Int) => Transform = {
    Transform({ case n: SequentialSegment => apply(n) }, _ , _, _)
  }
  
  
  implicit def funPar2Transform(apply: ParallelSegment => MusicalSegment): Transform = {
    Transform({ case n: ParallelSegment => apply(n) }, 1, 0, -1)
  }
  implicit def funParToTransformBuilderWPeriod(apply: ParallelSegment => MusicalSegment): Int => Transform = {
    Transform({ case n: ParallelSegment => apply(n) }, _, 0, -1)
  }
  implicit def funParToTransformBuilderWPeriodWFrom(apply: ParallelSegment => MusicalSegment): (Int, Int) => Transform = {
    Transform({ case n: ParallelSegment => apply(n) }, _ , _, -1)
  }
  implicit def funParToTransformBuilderWPeriodWFromWTo(apply: ParallelSegment => MusicalSegment): (Int, Int, Int) => Transform = {
    Transform({ case n: ParallelSegment => apply(n) }, _ , _, _)
  }
  
  def identity: Transform = Transform({ case m => m }, 1, 0, -1)
}