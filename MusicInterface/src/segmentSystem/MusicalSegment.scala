package segmentSystem

import rythmics.BPM
import tonalSystem.Tone

sealed trait MusicalSegment {
  def melody: List[MusicalSegment]
}

trait Parallel

trait Sequential

case class ParallelSegment(tracks: List[MusicalSegment]) extends MusicalSegment with Parallel {
  def melody = tracks
}
case class SequentialSegment(tracks: List[MusicalSegment]) extends MusicalSegment with Parallel {
  def melody = tracks
  
  def +(that: MusicalSegment) = SequentialSegment(List(this, that))
  def *(repetitions: Int): SequentialSegment = if (repetitions <= 1) this else {
    val half = repetitions/2
    (this * (repetitions-half)) + (this * (half))
  } 
}
case class Note(val tone: Tone, val duration: BPM) extends MusicalSegment with Parallel with Sequential {
  def melody = this :: Nil
}
