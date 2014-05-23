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
  implicit def tone2Note(tone: Tone)(implicit scale: Scale = Major(C)) = Note(tone, Q)
  implicit def tone2NoteBuilderWithDuration(tone: Tone)(implicit scale: Scale = Major(C)): BPM => Note = Note(tone, _)
}