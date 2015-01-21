package samples

import grammar.ImplicitsWords2Elements._
import rythmics.{H, Q, E, RythmCell}
import rythmics.RythmCell._
import tonalSystem.Tone
import tonalSystem.Tone.I

/** Calls to generation algorithm and details of melody producing
  * plus some helping function are implemented in Presentation trait
  * Extending element only has to define the four grammars :
  * chords, root, cells and tones
  * and call play() to compose and play the music.
  */
trait EndControlSpec extends VariationsSpec {

  /* specifies closing contition :
   * every grammar must be finished simultaneously
   */
  override val closeRoot = true
  override val closeCells = true
  override val closeTones = true

  // chords are inchanged since the grammar "decides" the end

  override
  lazy val root: Grammar[RootRythm] =
    (rootBody ** root) || rootEnd

  val rootBody: Grammar[RootRythm] = (Q ** Q) || H
  val rootEnd: Grammar[RootRythm] = H


  override
  lazy val cells: Grammar[RythmCell] = //RythmCell(Q::Nil) ** cells
    (cellsBody ** cells) || cellsEnd

  val cellsBody: Grammar[RythmCell] = (Q +: E +: E) || ((Q-) +: E)
  val cellsEnd: Grammar[RythmCell] = RythmCell(H::Nil)


  /* recall that inertialNext defined in InertialMelody is already
   * recursive and nullable
   */
  override
  lazy val tones = inertialNext(I) ** tonesEnd

  val tonesEnd: Grammar[Tone] = I

}

object EndControl extends App with EndControlSpec { play }
