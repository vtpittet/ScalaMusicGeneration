package generationTest

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

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter

import scala.util.Random


class GeneratorTest extends FunSuite with Matchers with BeforeAndAfter {

  lazy val chords: Grammar[Chord] =
    Triad(I) ** Triad(IV) ** Triad(V) ** Triad(I)

  lazy val root: Grammar[BPM] = H ** Q ** root

  lazy val cells: Grammar[RythmCell] = (Q +: E +: E) ** cells

  lazy val tones: Grammar[Tone] = gen(I)


  def gen(t: Tone): Grammar[Tone] = t ** (
    (gen(t decreaseBy 2), 2.0) ||
      (gen(t decreaseBy 1), 4.0) ||
      (gen(t), 1.0) ||
      (gen(t increaseBy 1), 4.0) ||
      (gen(t increaseBy 2), 2.0)
  )

  val initSol = Harm(
    ParsingTree(chords),
    ParsingTree(root),
    ParsingTree(cells),
    ParsingTree(tones),
    x => false
  )

  test("oneStepGen") {
    

    val candidates: List[Harm] = Generator.oneStepGen(initSol)

    candidates.size > 0 shouldBe true

  }

}
