package utils

import rythmics.BPM
import segmentSystem.Note
import tonalSystem.Tone
import rythmics.Q
import segmentSystem.ParallelSegment
import segmentSystem.SequentialSegment
import segmentSystem.MusicalSegment

/**
 * Keeps useful implicit definitions as conversion from tone to Note
 * Not in companion object to subsume Predef
 */
trait MelodyWriter {
  
  type SS = SequentialSegment
  type PS = ParallelSegment
  type MS = MusicalSegment
  type N = Note
  
  implicit val duration: BPM = Q
  
  // implicit conversions for easier melody description
  implicit def tone2Note(tone: Tone)(implicit duration: BPM) = Note(tone, Q)
  implicit def toneWithDuration2Note(td: (Tone, BPM)) = Note(td._1, td._2)
}