package samples

import grammar.ImplicitsWords2Elements._
import chord.{Chord, Triad}
import rythmics.{RythmCell, H, Q, E}
import rythmics.RythmCell._
import tonalSystem.Tone.{I, II, III, IV, V, VI, VII}
import tonalSystem.Tone


/** Calls to generation algorithm and details of melody producing
  * plus some helping function are implemented in Presentation trait
  * Extending element only has to define the four grammars :
  * chords, root, cells and tones
  * and call play() to compose and play the music.
  */
trait PurelyRandomSpec extends Presentation {

  // helping value
  private[this] val chords0: Grammar[Chord] =
    Triad(I) ** Triad(IV) ** Triad(V) ** Triad(I)

  // repeats chords0 three times in a sequence
  lazy val chords: Grammar[Chord] =
    repeat(3)(chords0) // chords0 ** chords0 ** chords0

  // only half notes
  lazy val root: Grammar[RootRythm] =
    H ** root

  // expression in parenthesis defines the rhythmic cell q-ee
  // q-ee <=> (quarter note, eight note, eight note)
  lazy val cells: Grammar[RythmCell] =
    (Q +: E +: E) ** cells


  // uniformly distributed tones of scale
  lazy val tones: Grammar[Tone] =
    (I || II || III || IV || V || VI || VII) ** tones
}

object PurelyRandom extends App with PurelyRandomSpec { play }
