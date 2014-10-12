package utils

import segmentSystem.SequentialSegment
import segmentSystem.MusicalSegment
import segmentSystem.Sequential

object SS {
 def apply(tracks: MusicalSegment*): SequentialSegment = Sequential(tracks.toList)
}