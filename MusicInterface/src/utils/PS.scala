package utils

import segmentSystem.ParallelSegment
import segmentSystem.MusicalSegment

object PS {
  def apply(tracks: MusicalSegment*): ParallelSegment = ParallelSegment(tracks.toList)
}