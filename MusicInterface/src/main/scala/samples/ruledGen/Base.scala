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


trait Base {


  lazy val noChords: Grammar[Chord] = FullChord(I)

  def nNoChords(n: Int): Grammar[Chord] = if (n > 0) noChords ** noChords ** noChords ** noChords ** nNoChords(n-1) else Triad(I) //Epsilon[Chord]()

  lazy val chords: Grammar[Chord] = Triad(I) ** (
    Triad(V) ||
    (Triad(IV) ** Triad(V)) ||
//    Seventh(V) ||
    (Triad(IV) ** Triad(V) ** Seventh(V) ** Triad(I))
  ) ** Triad(I)

  def nChords(n: Int): Grammar[Chord] = if (n > 0) chords ** nChords(n-1) else Epsilon[Chord]()

  lazy val root: Grammar[BPM] = (Q ** Q || H) ** ((root, 1.0) || (H, 1.0))
  lazy val cells: Grammar[RythmCell] = //RythmCell(Q::Nil) ** cells
    ((Q +: E +: E) || ((Q-) +: E)) ** ((cells, 1.0) || (Q, 0.5))

  lazy val tones: Grammar[Tone] = inertialGen(I) ** I

  def gen(t: Tone): Grammar[Tone] = (t ** (
    (gen(t decreaseBy 2), 2.0 * w(t)) ||
    (gen(t decreaseBy 1), 4.0 * w(t)) ||
    (gen(t), 1.0 * w(t)) ||
    (gen(t increaseBy 1), 4.0 * w(t)) ||
    (gen(t increaseBy 2), 2.0 * w(t)) ||
    (Epsilon[Tone](), 0.5)
  ))

  def inertialGen(t: Tone): Grammar[Tone] = {
    // positive if t2 higher, negative if t2 lower
    def d0(t2: Tone): Int = t stepsTo t2
    val pertFactor: Double = 1.0
    def perturbation : Int = (scala.util.Random.nextGaussian() * pertFactor).toInt
    // inertia factor
    // gravity factor
    // randomness
    def rec(t: Tone, inertia: Int, deviation: Int): Grammar[Tone] = {
//      print(inertia + ", ")
      val zeroTend = - d0(t) / 2
      val newInertia = inertia - deviation + perturbation + zeroTend
      val factorDown = if (newInertia > 0) 0.25 else 1.0
      val factorUp = if (newInertia < 0) 0.25 else 1.0
      val newT = t increaseBy deviation
      (newT) ** (
        (rec(newT, newInertia, -2), 2.0 * factorDown) ||
          (rec(newT, newInertia, -1), 4.0 * factorDown) ||
          (rec(newT, newInertia, 0), 1.0) ||
          (rec(newT, newInertia, 1), 4.0 * factorUp) ||
          (rec(newT, newInertia, 2), 2.0 * factorUp) ||
          (Epsilon[Tone](), 2.0)
      )
    }
    rec(t, 10, 0)
  }

  def w(t: Tone): Double = t match {
    case VII(_, _) => 0.1
    case t => 1/(1 + math.abs(t.octave))
  }

}

object BaseNoHarmonic extends App with Base {
  def music = Generator(nNoChords(8), root, cells, tones).generateMusic
  MelodyPlayer(music, 80)
}

object BaseSingleVoice extends App with Base {
  def music = Generator(nChords(4), root, cells, tones, true, true, true).generateMusic
  MelodyPlayer(music, 80)
}

object BaseWithAccompaniment extends App with Base {
  def music = Generator(nChords(4), root, cells, tones).generateMusicWithChords
  MelodyPlayer(music, 80)
}

