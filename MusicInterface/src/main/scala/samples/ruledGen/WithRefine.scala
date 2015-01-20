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


trait WithRefine {

  lazy val chords: Grammar[Chord] = Triad(I) ** (
    Triad(V) ||
    (Triad(IV) ** Triad(V)) ||
    (Triad(IV) ** Triad(V) ** Seventh(V))
  ) ** Triad(I)

  def nChords(n: Int): Grammar[Chord] = if (n > 0) chords ** nChords(n-1) else Epsilon[Chord]()
  
  lazy val root: Grammar[BPM] = (Q ** Q || H) ** ((root, 1.0) || (H, 1.0))
  lazy val cells: Grammar[RythmCell] =
    ((Q +: E +: E) || ((Q-) +: E)) ** ((cells, 1.0) || (Q, 0.5))

  lazy val tones: Grammar[Tone] = inertialGen(I) ** I

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
      val zeroTend = - d0(t)
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
    rec(t, 25, 0)
  }


  lazy val flatChords: Grammar[Chord] = Triad(I)
  lazy val flatRoot: Grammar[BPM] = H ** flatRoot
  lazy val flatCells: Grammar[RythmCell] = (Q +: Q) ** (flatCells || Q)
  lazy val flatTones: Grammar[Tone] = 
    ( I || III || V ) ** (flatTones || (I, 0.5))


  lazy val finalChords: Grammar[Chord] =
    ( nChords(3) **
      MelodyRefine[Chord](converge(10)) **
      nChords(2)
    )
//    flatChords ** flatChords ** flatChords ** flatChords

  def converge(n: Int): Grammar[Tone] =
    if (n < 2) converge(2)
    else {
      Production((for (i <- -n to n) yield (Word(I increaseBy i), 1.0)).toList) **
      converge(n-1)
    }

}

object WithRefineSingleVoice extends App with WithRefine {

//  println(converge(2))
//  for ( i <- 1 to 10) println(converge(i).firsts)
  val music = Generator(finalChords, root, cells, tones, true, true, true).generateMusic
  MelodyPlayer(music, 80)
}

object WithRefineWithAccom extends App with WithRefine {
  val music = Generator(finalChords, root, cells, tones, true, true, true).generateMusicWithChords
  MelodyPlayer(music, 80)
}

