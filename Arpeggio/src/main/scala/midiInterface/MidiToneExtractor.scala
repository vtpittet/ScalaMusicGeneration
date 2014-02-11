package main.scala.midiInterface

import main.scala.musical._

object MidiToneExtractor {
  private val c0 = 60
  private val octave = 12
  
  def apply(tone: Tone): Int = tone match {
    case A(o) => c0 - 3 + o * octave
    case AS(o) => c0 - 2 + o * octave
    case B(o) => c0 - 1 + o * octave
    case C(o) => c0 + 0 * octave
    case CS(o) => c0 + 1 + o * octave
    case D(o) => c0 + 2 + o * octave
    case DS(o) => c0 + 3 + o * octave
    case E(o) => c0 + 4 + o * octave
    case F(o) => c0 + 5 + o * octave
    case FS(o) => c0 + 6 + o * octave
    case G(o) => c0 + 7 + o * octave
    case GS(o) => c0 + 8 + o * octave
    case O => 0
  }
}