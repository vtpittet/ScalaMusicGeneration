package midiInterface

import javax.sound.midi.Sequence
import javax.sound.midi.Sequencer
import javax.sound.midi.Track
import javax.sound.midi.MidiSystem
import java.nio.ByteBuffer
import javax.sound.midi.MidiEvent
import javax.sound.midi.MetaMessage
import tonalSystem._
import javax.sound.midi.ShortMessage
import segmentSystem._
import rythmics.BPM


/**
 * Plays a given musicalSegment at fixed tempo in a provided scale, from the c
 */
case class MelodyPlayer(melody: MusicalSegment, bpm: Int, fromQN: Int = 0, toQN: Int = -1, instrument: Int = 0) {
  import MelodyPlayer._
  
  
  
  // push melody in sequencer
  val midiSequence: Sequence = new Sequence(Sequence.PPQ, DEFAULT_RESOLUTION)
  val midiTrack: Track = midiSequence.createTrack
  // set Tempo
  setTempo
  setInstrument
  // put events in midiTrack
  // first noteoff then note on to avoid note erasure bug
  
  createEventsPerSegment(melody, -fromQN, false)
  createEventsPerSegment(melody, -fromQN, true)
  // play the melody
  play
  
  def setTempo {
	val metaMessage = new MetaMessage()
	metaMessage.setMessage(0x51, getMPQN(bpm), 3)
    midiTrack.add(new MidiEvent(metaMessage, 0))
    def getMPQN(bpm: Int): Array[Byte] = {
      val microsecondsPerMinute = 60000000
      val mpqn: Int = Math.max(Math.min(microsecondsPerMinute / bpm, 8355711), 0)
      val fourBytesArray = ByteBuffer.allocate(4).putInt(mpqn).array()
      Array[Byte](fourBytesArray(1), fourBytesArray(2), fourBytesArray(3))
    }
  }
  
  def setInstrument = {
     val instrumentChange = new ShortMessage()
     instrumentChange.setMessage(ShortMessage.PROGRAM_CHANGE, 0, instrument ,0)
     midiTrack.add(new MidiEvent(instrumentChange,0))
   }
  
  def createEventsPerSegment(segt: MusicalSegment, segtTimeStamp: Double, noteOn: Boolean): Double = segt match {
    case ParallelSegment(melody) =>
      (for(segt <- melody) yield createEventsPerSegment(segt, segtTimeStamp, noteOn)).foldLeft(0.0)((x, y) => scala.math.max(x, y))
    case SequentialSegment(melody) =>
      melody.foldLeft(segtTimeStamp)((x, y) => createEventsPerSegment(y, x, noteOn))
    case n @ Note(tone, duration) =>
      createEventsPerNote(tone, duration, n.scale, segtTimeStamp, noteOn)
    /*
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
    * 
    */
  }
  
  def createEventsPerNote(tone: Tone, duration: BPM, scale: Scale, segtTimeStamp: Double, noteOn: Boolean): Double = {
    // if segtTimeStamp is in given interval (toQN == -1 => no upperbound)
    if (segtTimeStamp >= 0 && (segtTimeStamp < toQN-fromQN || toQN == -1)) {
	  val shortMessage = new ShortMessage()
	  shortMessage.setMessage(
	          ShortMessage.NOTE_ON, 0, MidiToneExtractor(scale.pitchTone(tone)),
			  DEFAULT_VELOCITY)
      if (noteOn) midiTrack.add(new MidiEvent(shortMessage,
              (segtTimeStamp * DEFAULT_RESOLUTION).toLong))
      else {
		val shortMessage = new ShortMessage()
		shortMessage.setMessage(ShortMessage.NOTE_OFF, 0, MidiToneExtractor(scale.pitchTone(tone)),
                DEFAULT_VELOCITY)
	    midiTrack.add(new MidiEvent(shortMessage,
                ((segtTimeStamp + duration.computed) * DEFAULT_RESOLUTION).toLong))
	  }
    
    }
    segtTimeStamp + duration.computed
  }
  
  /*
  def createOffEvents(tone: Tone, timeStamp: Double): Unit = {
    midiTrack.add(new MidiEvent(new ShortMessage(
          ShortMessage.NOTE_OFF, 0, MidiToneExtractor(tone), MelodyReader.DEFAULT_VELOCITY),
          (timeStamp * MelodyReader.DEFAULT_RESOLUTION).toLong))
  }
  * 
  */
  
  def play {
    val sequencer: Sequencer = MidiSystem.getSequencer
    sequencer.open
    sequencer.setSequence(midiSequence)
    sequencer.start
    println("Running ...")
    var prevTs = System.currentTimeMillis
    while(sequencer.isRunning()) {
      var ct = System.currentTimeMillis
      if (ct % 1000 == 0 && ct != prevTs) {
        prevTs = ct
        println("Running ...")
      }
    }
    println("done!")
    sequencer.close()
  }
}

object MelodyPlayer {
  val DEFAULT_RESOLUTION: Int = 16
  val DEFAULT_VELOCITY: Int = 60
//  def apply(melody: MusicalSegment, bpm: Int, scale: Scale, fromQN: Int = 0, toQN: Int = -1): MelodyPlayer =
//    MelodyPlayer((melody, scale) :: Nil, bpm, fromQN, toQN)
//  def apply(bpm: Int, melody: (MusicalSegment, Scale)*): MelodyPlayer = MelodyPlayer(melody.toList, bpm, 0, -1)
//  def apply(bpm: Int, fromQN: Int, toQN: Int, melody: (MusicalSegment, Scale)*): MelodyPlayer = MelodyPlayer(melody.toList, bpm, fromQN, toQN)
}
