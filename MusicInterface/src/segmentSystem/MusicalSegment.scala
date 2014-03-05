package segmentSystem

import rythmics.BPM
import tonalSystem.Tone

sealed trait MusicalSegment {
  def melody: List[MusicalSegment]
}

case class ParallelSegment(tracks: List[MusicalSegment]) extends MusicalSegment{
  def melody = tracks
  
  def *(repetitions: Int): ParallelSegment = ParallelSegment(List.fill(repetitions)(this))
}
case class SequentialSegment(tracks: List[MusicalSegment]) extends MusicalSegment{
  def melody = tracks
  
  def +(that: MusicalSegment) = SequentialSegment(List(this, that))
  def *(repetitions: Int): SequentialSegment = SequentialSegment(List.fill(repetitions)(this))
}
case class Note(val tone: Tone, val duration: BPM) extends MusicalSegment{
  def melody = this :: Nil
}
