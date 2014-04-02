package utils

import segmentSystem.SequentialSegment
import segmentSystem.MusicalSegment

object SS {
 def apply(tracks: MusicalSegment*): SequentialSegment = SequentialSegment(tracks.toList)
}