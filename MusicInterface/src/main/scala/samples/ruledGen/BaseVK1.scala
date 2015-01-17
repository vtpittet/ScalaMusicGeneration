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


trait BaseVK1 {

  lazy val noChords: Grammar[Chord] = FullChord(I)

  def nNoChords(n: Int): Grammar[Chord] = if (n > 0) noChords ** noChords ** noChords ** noChords ** nNoChords(n-1) else Epsilon[Chord]()

  lazy val chords: Grammar[Chord] = 
//    Triad(I) ** Triad(IV) ** Triad(V) ** Triad(I)
    Triad(I) ** Seventh(V) ** Triad(I) ** Triad(VI) ** Triad(V) **
    Seventh(V) ** Triad(I)

/*
** Triad(I) ** Triad(IV) ** Triad(V) ** 
  Triad(VI) ** Triad(I) ** Seventh(V) ** Triad(I)
*/

  def nChords(n: Int): Grammar[Chord] = if (n > 0) chords ** nChords(n-1) else Epsilon[Chord]()

  lazy val root: Grammar[BPM] = ((H ** Q ** Q, 2.0) || 
				 (Q ** Q ** Q ** Q) ||
			          (Q ** Q ** H, 1.5))  ** root

  lazy val cells: Grammar[RythmCell] = (Q +: E +: E) ** cells
    //((Q +: E +: E, 0.5) || 
    //				(E +: E +: E +: E, 0.2)) ** cells

  lazy val tones: Grammar[Tone] = gen(I)

  def gen(t: Tone): Grammar[Tone] = (t ** (
    (gen(t decreaseBy 2), 1.0 * w(t)) ||
    (gen(t decreaseBy 1), 4.0 * w(t)) ||
    (gen(t), 1.0 * w(t)) ||
    (gen(t increaseBy 1), 4.0 * w(t)) ||
    (gen(t increaseBy 2), 1.0 * w(t))
  ))

  def w(t: Tone): Double = 1/(1 + math.abs(t.octave))

}

/*
object BaseNoHarmonic extends App with BaseVK1 {
  def music = Generator(nNoChords(8), root, cells, tones).generateMusic
  MelodyPlayer(music, 80)
}

object BaseSingleVoice extends App with BaseVK1 {
  def music = Generator(nChords(2), root, cells, tones).generateMusic
  MelodyPlayer(music, 80)
}
*/

object BaseWithAccompanimentVK1 extends App with BaseVK1 {
  def music = Generator(nChords(4), root, cells, tones).generateMusicWithChords
  MelodyPlayer(music, 80)
}

