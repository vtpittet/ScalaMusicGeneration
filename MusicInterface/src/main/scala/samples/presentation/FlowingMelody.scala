package samples.presentation

import tonalSystem.Tone
import tonalSystem.Tone.I
import grammar.ImplicitsWords2Elements._

/** Calls to generation algorithm and details of melody producing
  * plus some helping function are implemented in Presentation trait
  * Extending element only has to define the four grammars :
  * chords, root, cells and tones
  * and call play() to compose and play the music.
  */
trait FlowingMelodySpec extends PurelyRandomSpec {

  override
  lazy val tones: Grammar[Tone] = nextTone(I)

  /* helping abstraction to generate a tone then
   * choose next one according to previous one
   */
  def nextTone(t: Tone): Grammar[Tone] =
    t ** (
      (nextTone(t decreaseBy 2), 1.0) ||
      (nextTone(t decreaseBy 1), 2.0) ||
      (nextTone(t), 0.5)              ||
      (nextTone(t increaseBy 1), 2.0) ||
      (nextTone(t increaseBy 2), 1.0)
    )
}

object FlowingMelody extends App with FlowingMelodySpec { play }
