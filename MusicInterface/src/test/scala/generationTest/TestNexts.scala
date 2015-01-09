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

  test("Hello world") {
    def gen(t: Tone): Grammar[Tone] = t ** (
      (gen(t decreaseBy 2), 2.0) ||
      (gen(t decreaseBy 1), 2.0) ||
      (gen(t), 1.0) ||
      (gen(t increaseBy 1), 4.0) ||
      (gen(t increaseBy 2), 2.0)
    )

    lazy val tones: Grammar[Tone] = gen(II)

    val tree = ParsingTree(tones)

    val next = tree.nexts(x => true).head

    val nnexts = tree.nexts(x => true) flatMap (_.nexts(x => true))

    for(n <- nnexts) println(n.collectWords)

    //println(nexts.size)
    
//    println(next.collectWords)


    // TODO OOOOOOOOOOOOOOOOOOOOOOOOOOOOOO y aurait-il un word.head qui se repercute ???
    // TODO OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO toString problem with parametric grammar will cause same problem in nullable and firsts

  }

  test("Loop nullable") {
    def gram: Grammar[Tone] = eps[Tone] ** gram

    gram.nullable shouldBe false
  }
}
