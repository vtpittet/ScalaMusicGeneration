package segmentSystem

import rythmics.BPM
import tonalSystem.Tone

sealed trait MusicalSegment {
  def melody: List[MusicalSegment]
  
  // parallel addition
  def |(that: MusicalSegment): ParallelSegment = ParallelSegment(List(this, that))
  // sequential addition
  def *(that: MusicalSegment): SequentialSegment = SequentialSegment(List(this, that))
  
//  Note that parallel multiplication is idempotent as the same melody is replayed at the same time
//  def *(repetitions: Int): ParallelSegment = ParallelSegment(List.fill(repetitions)(this))
  
  def *(repetitions: Int): SequentialSegment = SequentialSegment(List.fill(repetitions)(this))
  
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
  
  def +(toneRise: Int): Note = Note(tone + toneRise, duration)
}
