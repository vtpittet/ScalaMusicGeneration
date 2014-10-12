package segmentSystem

import tonalSystem.Scale

trait MusicalSegmentLike[+Repr <: MusicalSegmentLike[Repr] with MusicalSegment] {
  self: Repr =>
  
  val buildFromMelody: List[MusicalSegment] => Repr
  
  def groupBy(size: Int): Repr = if (melody.size <= 2) self else {
    buildFromMelody(melody.grouped(size).map(buildFromMelody(_)).toList)
  }
}