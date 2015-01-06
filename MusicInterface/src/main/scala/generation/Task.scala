package generation

import grammar.GrammarElement

/** task represents what has to be accomplished by parsing tree
  * This additionnal layer is useful to allow parsing trees to be
  * assigned tasks not being generated by theirs grammars (hence disallowin
  * confusing possibility of grammars containing meaning less elements)
  */
trait Task[A]

case class Refine[A](ge: GrammarElement[A]) extends Task[A]