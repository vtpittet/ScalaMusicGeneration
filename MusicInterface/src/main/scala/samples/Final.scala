package samples

import generation.Generator
import segmentSystem.MusicalSegment

/** Calls to generation algorithm and details of melody producing
  * plus some helping function are implemented in Presentation trait
  * Extending element only has to define the four grammars :
  * chords, root, cells and tones
  * and call play() to compose and play the music.
  */
trait FinalSpec extends ConvergentRefinementSpec {
  override val useGenerator: Generator => MusicalSegment = _.generateMusicWithChords
}

object Final extends App with FinalSpec { play }
