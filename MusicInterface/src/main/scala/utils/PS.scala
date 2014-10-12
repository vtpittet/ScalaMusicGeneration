package utils

import segmentSystem.ParallelSegment
import segmentSystem.MusicalSegment
import segmentSystem.Parallel

object PS {
  def apply(tracks: MusicalSegment*): ParallelSegment = Parallel(tracks.toList)
}