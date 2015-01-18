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


class PartialSolTest extends FunSuite with Matchers with BeforeAndAfter {

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

  val default: PartialSolution[_] => Boolean = x => false
  val initSol = Harm(
    ParsingTree(chords),
    ParsingTree(root),
    ParsingTree(cells),
    ParsingTree(tones),
    default
  )

  test("dispatch id under no messages") {
    val (h, rr, rc, m) = initSol.dispatchMsgs
    initSol shouldBe Harm(h, rr, rc, m, default)
  }

  test("gen test") {
    
    val candidates = initSol.gen

    candidates.size > 0 shouldBe true

  }
}
