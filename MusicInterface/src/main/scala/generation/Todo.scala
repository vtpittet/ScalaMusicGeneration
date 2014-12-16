package generation

import grammar.GrammarElement
import chord.Chord
import rythmics.BPM
import tonalSystem.Tone

sealed trait Todo[A]

case class Generate[A](ge: GrammarElement[A]) extends Todo[A]

case class Refine[A](ge: GrammarElement[A]) extends Todo[A]

sealed trait Message extends Todo[Nothing] {
  val message: Todo[_]
}

sealed trait RefineMessage[A] extends Message {
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


sealed trait InsertMessage[A] extends Message {
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

