package grammar

import chord._
import rythmics._
import tonalSystem._

/** import content of this object for full featured writing
  */
object ImplicitsWords2Elements {

  type Grammar[A] = GrammarElement[A]

  implicit def tuple2Prod[A](tpl: (GrammarElement[A], Double)): Production[A] = 
    Production(tpl::Nil)

  private def tupleWord2Prod[A](tpl: (A, Double)): Production[A] = {
    val (word, weight) = tpl
    Production((Word(word), weight) :: Nil)
  }

  implicit def tupleChord2Prod(tpl: (Chord, Double)) = tupleWord2Prod[Chord](tpl)
  implicit def tupleBPM2Prod(tpl: (BPM, Double)) = tupleWord2Prod[BPM](tpl)
  implicit def tupleRCell2Prod(tpl: (RythmCell, Double)) = tupleWord2Prod[RythmCell](tpl)
  implicit def tupleMel2Prod(tpl: (Tone, Double)) = tupleWord2Prod[Tone](tpl)



  private def wordVal2WordElt[A](word: A): Word[A] = Word(word)

  implicit def chord2WordElt(word: Chord) = wordVal2WordElt[Chord](word)
  implicit def bpm2WordElt(word: BPM) = wordVal2WordElt[BPM](word)
  implicit def rCell2WordElt(word: RythmCell) = wordVal2WordElt[RythmCell](word)
  implicit def tone2WordElt(word: Tone) = wordVal2WordElt[Tone](word)



}
