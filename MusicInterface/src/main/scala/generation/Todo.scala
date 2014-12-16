package generation

import grammar.GrammarElement
import chord.Chord
import rythmics.BPM
import tonalSystem.Tone

trait Todo[A]

case class Generate[A](ge: GrammarElement[A]) extends Todo[A]

case class Refine[A](ge: GrammarElement[A]) extends Todo[A]

trait Message[A] extends Todo[Nothing] {
  val message: Todo[A]
}

trait RefineMessage[A] extends Message[A] {
  val refinement: GrammarElement[A]
  val message = Refine(refinement)
}

case class HarmRefine(refinement: GrammarElement[Chord])
    extends RefineMessage[Chord]

case class RootRythmRefine(refinement: GrammarElement[BPM])
    extends RefineMessage[BPM]

case class RythmRefine(refinement: GrammarElement[BPM])
    extends RefineMessage[BPM]

case class MelodyRefine(refinement: GrammarElement[Tone])
    extends RefineMessage[Tone]


trait InsertMessage[A] extends Message[A] {
  val insertion: GrammarElement[A]
  val message = Generate(insertion)
}



case class HarmInsert(insertion: GrammarElement[Chord])
    extends InsertMessage[Chord]

case class RootRythmInsert(insertion: GrammarElement[BPM])
    extends InsertMessage[BPM]

case class RythmInsert(insertion: GrammarElement[BPM])
    extends InsertMessage[BPM]

case class MelodyInsert(insertion: GrammarElement[Tone])
    extends InsertMessage[Tone]

