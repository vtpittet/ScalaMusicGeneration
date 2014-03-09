package segmentSystem

import rythmics.BPM
import tonalSystem.Tone
import tonalSystem.O

sealed trait MusicalSegment {
  def melody: List[MusicalSegment]
  
  // parallel addition
  def |(that: MusicalSegment): ParallelSegment = ParallelSegment(List(this, that))
  // sequential addition
  def +(that: MusicalSegment): SequentialSegment = SequentialSegment(List(this, that))
  
  
  /*
   * Note that parallel multiplication is nearly idempotent as the same melody is replayed at the same time
   * def *(repetitions: Int): ParallelSegment = ParallelSegment(List.fill(repetitions)(this))
   */
  def *(repetitions: Int): SequentialSegment = SequentialSegment(List.fill(repetitions)(this))
  
//  TODO is it good to assume identity at each beginning of definition ?
  def *+(transf: (MusicalSegment) => MusicalSegment*): SequentialSegment = 
    multiTransf(transf, SequentialSegment(_))
  def *|(transf: (MusicalSegment) => MusicalSegment*): ParallelSegment = 
    multiTransf(transf, ParallelSegment(_))
  
  private def multiTransf[T <: MusicalSegment](transf: Seq[(MusicalSegment) => MusicalSegment], builder: (List[MusicalSegment]) => T): T = {
    val iter = (((x: MusicalSegment) => x) +: transf).iterator
    builder(List.fill(transf.size + 1)(iter.next()(this)))
  }
  
  def >>(duration: BPM*): SequentialSegment = SequentialSegment(duration.foldRight(this :: Nil)((d, s) => O(d) :: s))
  
  
  def length: Double = melody.foldLeft(0.0)((x, y) => x + y.length)
  
  def +(toneRise: Int): MusicalSegment
  
}

case class ParallelSegment(tracks: List[MusicalSegment]) extends MusicalSegment{
  def melody = tracks
  
  def +(toneRise: Int): ParallelSegment = ParallelSegment(melody.map(_ + toneRise))
}
case class SequentialSegment(tracks: List[MusicalSegment]) extends MusicalSegment{
  def melody = tracks
  
  def +(toneRise: Int): SequentialSegment = SequentialSegment(melody.map(_ + toneRise))
}
case class Note(val tone: Tone, val duration: BPM) extends MusicalSegment{
  def melody = this :: Nil
  override def length = duration.computed
  def +(toneRise: Int): Note = Note(tone + toneRise, duration)
}
