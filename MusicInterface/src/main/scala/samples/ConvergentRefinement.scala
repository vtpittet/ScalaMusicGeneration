package samples

import grammar.ImplicitsWords2Elements._
import chord.Chord
import tonalSystem.Tone
import tonalSystem.Tone.I
import grammar.{Word, Production, MelodyRefine}

/** Calls to generation algorithm and details of melody producing
  * plus some helping function are implemented in Presentation trait
  * Extending element only has to define the four grammars :
  * chords, root, cells and tones
  * and call play() to compose and play the music.
  */
trait ConvergentRefinementSpec extends EndControlSpec {

  // recall that chords0 was defined in Variations
  override
  lazy val chords: Grammar[Chord] =
    ( repeat(3)(chords0) **                // beginning
      MelodyRefine[Chord](converge(10)) ** // near the end, send refinement message
      repeat(2)(chords0))                  // end


  // creates a n infinite grammar that converges in n steps
  def converge(n: Int): Grammar[Tone] =
    // still allows some oscillation after converged
    if (n < 2) converge(2)
    else {
      Production((for (i <- -n to n) yield (Word(I increaseBy i), 1.0)).toList) **
      converge(n-1)
    }
}

object ConvergentRefinement extends App with ConvergentRefinementSpec { play }
