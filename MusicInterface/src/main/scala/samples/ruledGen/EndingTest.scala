package samples.ruledGen

import grammar._
import grammar.ImplicitsWords2Elements._
import generation._
import chord._
import rythmics._
import rythmics.RythmCell
import rythmics.RythmCell._
import tonalSystem.Tone.{I, II, III, IV, V, VI, VII}
import tonalSystem.Tone
import midiInterface.MelodyPlayer
import tonalSystem.Minor
import tonalSystem.C


object EndingTest extends App {
  lazy val chords: Grammar[Chord] = Triad(I) ** Triad(I) ** Triad(I) ** Triad(I) ** Triad(I) ** Triad(I)

  lazy val root: Grammar[BPM] = Q ** (root || W)

  lazy val cells: Grammar[RythmCell] = (Q +: Q) ** (cells || (E +: (Q-)))

  lazy val tones: Grammar[Tone] = I ** II ** (tones || (III ** I))

  def music = Generator(chords, root, cells, tones, false, false, false).generateMusic
  MelodyPlayer(music, 80)

}
