package main.scala

import main.scala.musical.Melody
import main.scala.musical.Chord
import main.scala.musical.BasicChord
import main.scala.musical.Tone
import main.scala.musical.Arpeggio
import main.scala.midiInterface.MelodyReader
import main.scala.musical._
import main.scala.musical.SilentChord

case class FJCMelody(tonic: Tone, val tempo: Int) extends Melody {
  
  val arp1: Arpeggio = Arpeggio(List(List(0, 3/2.0), List(1/2.0), List(1)), 2)
  val chord1: Chord = BasicChord(List(tonic, tonic+2, tonic+4), arp1) 
  
  val arp2: Arpeggio = Arpeggio(List(List(0), List(1/2.0), List(1)), 2)
  val chord2: Chord = BasicChord(List(tonic+4, tonic+5, tonic+7), arp2)
  
  val arp3: Arpeggio = Arpeggio(List(List(0, 2/4.0), List(1/4.0), List(3/4.0), List(1), List(1.5)), 2)
  val chord3: Chord = BasicChord(List(tonic+7, tonic+9, tonic+5, tonic+4, tonic), arp3)
  
  val arp4: Arpeggio = Arpeggio(List(List(0, 1), List(1/2.0)), 2)
  val chord4: Chord = BasicChord(List(tonic, tonic-5), arp4)
  
  val chord0: Chord = SilentChord(2)
  
  val track1 = List(chord1, chord1, chord2, chord2, chord3, chord3, chord4, chord4)
  
  val track2 = chord0 :: chord0 :: track1
  
  val track3 = chord0 :: chord0 :: track2
  
  val tracks = List(track1, track2, track3)

  
}

object FJCMelody {
  def main(args: Array[String]) {
    MelodyReader(FJCMelody(E(0), 200))
  }
}