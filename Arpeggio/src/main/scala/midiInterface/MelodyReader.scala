package main.scala.midiInterface

import java.nio.ByteBuffer
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import javax.sound.midi.Track
import main.scala.musical.Chord
import main.scala.musical.Melody
import main.scala.musical.Tone
import javax.sound.midi.Sequencer
import javax.sound.midi.MidiSystem

class MelodyReader(melody: Melody) {
  
  
// for now only consider track 0 in melody
  
  // push melody in sequencer
  val midiSequence: Sequence = new Sequence(Sequence.PPQ, MelodyReader.DEFAULT_RESOLUTION)
  val midiTrack: Track = midiSequence.createTrack
  val bpm = melody.tempo
  // set Tempo
  setTempo
  // put events in midiTrack
  for (track <- melody.tracks) createEventsPerTrack(track, 0)
  // play the melody
  play
  
  def setTempo {
    midiTrack.add(new MidiEvent(new MetaMessage(0x51, getMPQN(bpm), 3), 0))
    def getMPQN(bpm: Int): Array[Byte] = {
      val microsecondsPerMinute = 60000000
      val mpqn: Int = Math.max(Math.min(microsecondsPerMinute / bpm, 8355711), 0)
      val fourBytesArray = ByteBuffer.allocate(4).putInt(mpqn).array()
      Array[Byte](fourBytesArray(1), fourBytesArray(2), fourBytesArray(3))
    }
  }
  
  def createEventsPerTrack(track: List[Chord], chordTimeStamp: Double): Unit = track match {
    case chord :: Nil => {
      // Not neede case, let notes be at the end of melody
      for (note <- chord.tones.zip(chord.arpeggio.notesStarts)) createEventsPerNote(note, chordTimeStamp, chord.arpeggio.duration)
      for (tone <- chord.tones) createOffEvents(tone, chordTimeStamp + 2 * chord.arpeggio.duration)
    }
    case chord :: chords => {
      for (note <- chord.tones.zip(chord.arpeggio.notesStarts)) createEventsPerNote(note, chordTimeStamp, chord.arpeggio.duration)
      for (tone <- chord.tones) createOffEvents(tone, chordTimeStamp + chord.arpeggio.duration)
      createEventsPerTrack(chords, chordTimeStamp + chord.arpeggio.duration)
    }
    case Nil =>
  }
  
  def createEventsPerNote(note: (Tone, List[Double]),
        chordTimeStamp: Double,
        chordDuration: Double): Unit = note._2 match {
    case start :: starts => {
      // println("ChordTimeStamp : " + chordTimeStamp)
      // println("start : " + start)
      midiTrack.add(new MidiEvent(new ShortMessage(
            ShortMessage.NOTE_ON, 0, MidiToneExtractor(note._1), MelodyReader.DEFAULT_VELOCITY),
            ((chordTimeStamp + start) * MelodyReader.DEFAULT_RESOLUTION).toLong))
      if (chordDuration > 0) {
        createEventsPerNote((note._1, starts), chordTimeStamp, chordDuration - start)
      }
    }
    case Nil =>
  }
  
  def createOffEvents(tone: Tone, timeStamp: Double): Unit = {
    midiTrack.add(new MidiEvent(new ShortMessage(
          ShortMessage.NOTE_OFF, 0, MidiToneExtractor(tone), MelodyReader.DEFAULT_VELOCITY),
          (timeStamp * MelodyReader.DEFAULT_RESOLUTION).toLong))
  }
  
  def play {
    val sequencer: Sequencer = MidiSystem.getSequencer
    sequencer.open
    sequencer.setSequence(midiSequence)
    sequencer.start
    println("Running ..")
    while(sequencer.isRunning()) if (System.currentTimeMillis % 1000 == 0) {
      println("Running ...")
    }
    println("done!")
    sequencer.close()
  }
}

object MelodyReader {
  val DEFAULT_RESOLUTION: Int = 16
  val DEFAULT_VELOCITY: Int = 60
}