package utils

import rythmics.BPM
import segmentSystem.Note
import tonalSystem.Tone
import rythmics.Q

/**
 * Keeps useful implicit definitions as conversion from tone to Note
 * Not in companion object to subsume Predef
 */
trait MelodyWriter {
  
  implicit val duration: BPM = Q
  
  // implicit conversions for easier melody description
  implicit def tone2Note(tone: Tone)(implicit duration: BPM) = Note(tone, Q)
}