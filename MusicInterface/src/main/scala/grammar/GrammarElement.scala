package grammar
import generation.PrefixOperator


// TODO specify composition rules
// How to allow regex notation inside bodies ? (not compulsory)


/** common top trait for grammar interface
  */
sealed trait GrammarElement[A]

/** trait used to regroup word and epilon as a common
  * case when generating sentences
  */
sealed trait Terminal[A] extends GrammarElement[A] with PrefixOperator {
  val arity = 0
}

/** Word represents the non-epsilon terminal values of the grammar
  * Usually called Character of an alphabet, it would be confusing
  * with the class Char
  */
case class Word[A](value: A) extends Terminal[A]

/** Epsilon terminal of grammar
  */
case object Epsilon extends Terminal[Nothing]

/** A Rule is a sequential list of grammar elements
  */
case class Rule[A](body: List[GrammarElement[A]]) extends GrammarElement[A] with PrefixOperator {
  lazy val arity = body.size
}

/** A production is a list of possible weighted rules. one has to be chosen
  * in order to generate with the grammar
  */
case class Production[A](rules: List[(Rule[A], Double)]) extends GrammarElement[A]


