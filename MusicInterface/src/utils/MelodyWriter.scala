package utils

import rythmics.BPM
import segmentSystem.Note
import tonalSystem.Tone
import rythmics.Q
import segmentSystem.ParallelSegment
import segmentSystem.SequentialSegment
import segmentSystem.MusicalSegment
import tonalSystem.Major
import tonalSystem.Scale
import tonalSystem.C
import segmentSystem.Parallel
import segmentSystem.Sequential

/**
 * Keeps useful implicit definitions as conversion from tone to Note
 * Not in companion object to subsume Predef
 */
trait MelodyWriter {
  
  type SS = SequentialSegment
  type PS = ParallelSegment
  type MS = MusicalSegment
  type N = Note
  
  // implicit conversions for easier melody description
  implicit def tone2Note(tone: Tone)(implicit
        noteDuration: BPM = Q,
        scale: Scale = Major(C),
        parallelBuilder: List[MusicalSegment] => ParallelSegment = Parallel(_),
        sequentialBuilder: List[MusicalSegment] => SequentialSegment = Sequential(_)) = {
    Note(tone, noteDuration)
  }
  implicit def tone2NoteBuilderWithDuration(tone: Tone)(implicit
        scale: Scale = Major(C),
        parallelBuilder: List[MusicalSegment] => ParallelSegment = Parallel(_),
        sequentialBuilder: List[MusicalSegment] => SequentialSegment = Sequential(_)): BPM => Note = Note(tone, _)
}