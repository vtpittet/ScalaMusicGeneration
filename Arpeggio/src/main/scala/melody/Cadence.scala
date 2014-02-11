package main.scala.melody

import main.scala.musical.Arpeggio
import main.scala.musical.Tone
import main.scala.musical.MajorChord
import main.scala.musical.Melody
import main.scala.musical.Chord

case class Cadence(tonic: Tone, arpeggio: Arpeggio, val tempo: Int) extends Melody {
  val subdominant: Tone = tonic + 5
  val dominant: Tone = tonic + 7
  val chordDuration = arpeggio.duration
  
  val tonicChord: Chord = new MajorChord(tonic, arpeggio)
  val subdominantChord: Chord = new MajorChord(subdominant, arpeggio)
  val dominantChord: Chord = new MajorChord(dominant, arpeggio)
  
  val singleTrack: Track = List(tonicChord, subdominantChord, dominantChord, tonicChord)
  val tracks = singleTrack :: Nil
}