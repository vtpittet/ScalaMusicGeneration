package midiInterface

import tonalSystem._

object MidiToneExtractor {
  private val c0 = 60
  private val octave = 12
  
  def apply(tone: Pitch): Int = tone match {
    case A(o, a) => c0 - 3 + o * octave + a
    case B(o, a) => c0 - 1 + o * octave + a
    case C(o, a) => c0 + 0 + o * octave + a
    case D(o, a) => c0 + 2 + o * octave + a
    case E(o, a) => c0 + 4 + o * octave + a
    case F(o, a) => c0 + 5 + o * octave + a
    case G(o, a) => c0 + 7 + o * octave + a
    case S => 0
  }
}