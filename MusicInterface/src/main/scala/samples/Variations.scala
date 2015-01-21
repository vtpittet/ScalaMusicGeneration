package samples

import tonalSystem.Tone
import tonalSystem.Tone.{I, IV, V}
import grammar.ImplicitsWords2Elements._
import rythmics.{H, Q, E, RythmCell}
import rythmics.RythmCell._
import chord.{Chord, Triad, Seventh}


/** Calls to generation algorithm and details of melody producing
  * plus some helping function are implemented in Presentation trait
  * Extending element only has to define the four grammars :
  * chords, root, cells and tones
  * and call play() to compose and play the music.
  */
trait VariationsSpec extends InertialMelodySpec {

  val chords0: Grammar[Chord] =
    Triad(I) **
      ( Triad(V) ||
        Triad(IV) ** Triad(V) ||
        Triad(IV) ** Triad(V) ** Seventh(V)
      ) ** Triad(I)

  override
  lazy val chords: Grammar[Chord] =
    repeat(3)(chords0)

  override
  lazy val root: Grammar[RootRythm] =
    ((Q ** Q) || H) ** root

  override
  lazy val cells: Grammar[RythmCell] =
    ( (Q +: E +: E) || ((Q-) +: E) ) ** cells

}

object Variations extends App with VariationsSpec { play }
