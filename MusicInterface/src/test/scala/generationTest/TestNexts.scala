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


class TestNexts extends FunSuite with Matchers with BeforeAndAfter {

  def eps[A]: Epsilon[A] = GrammarElement.epsilon[A]


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

  test("nexts on harm") {
    val tree = ParsingTree(chords)
    val nexts: List[ParsingTree[Chord]] = tree.nexts(x => true, false)

    nexts.size > 0 shouldBe true
  }

  test("nexts on rule > 2") {
    val gramm: Grammar[Tone] = I ** IV ** V
    println(gramm)
    val tree = ParsingTree(gramm)
    val nexts = tree.nexts(x => true, false)

    nexts.size > 0 shouldBe true
  }

  test("Base Test") {

    val tree = ParsingTree(tones)


    val nexts: List[ParsingTree[Tone]] = tree.nexts(x => true, false)

    nexts.size shouldBe 5

    nexts.toSet flatMap { n: ParsingTree[Tone] => n.nextWords } shouldBe Set(
      VI(-1, None), VII(-1,  None), I(0, None), II(0, None), III(0, None)
    )

    for(n <- nexts) n.lastWord shouldBe Some(I)

  }

  test("Loop nullable") {
    def gram: Grammar[Tone] = eps[Tone] ** gram

    gram.nullable shouldBe false
  }

}
