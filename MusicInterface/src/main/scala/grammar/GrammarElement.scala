package grammar
import generation.{PrefixOperator, Task}
import chord.Chord
import rythmics.BPM
import tonalSystem.Tone

// TODO specify composition rules
// How to allow regex notation inside bodies ? (not compulsory)


/** common top trait for grammar interface
  */
sealed trait GrammarElement[A] extends Task[A] {
  def orComposition(that: =>GrammarElement[A], weight: Double): Production[A]
  def andComposition(that: =>GrammarElement[A]): Rule[A]

  val toName: String
  def rToString(callers: List[AnyRef]): String
  override lazy val toString: String = rToString(Nil)

  def ||(that: =>GrammarElement[A], weight: Double): Production[A] =
    orComposition(that, weight)
  def ||(word: A, weight: Double): Production[A] =
    orComposition(Word(word), weight)
  def **(that: =>GrammarElement[A]): Rule[A] =
    andComposition(that)
  def **(word: A): Rule[A] =
    andComposition(Word(word))

  def compareBody(that: GrammarElement[A]): Boolean

  lazy val nullable: Boolean = rNullable(Nil)
  def rNullable(callers: List[GrammarElement[A]]): Boolean


  lazy val firsts: Set[A] = rFirsts(Nil)
  def rFirsts(callers: List[GrammarElement[A]]): Set[A]
}

object GrammarElement {
  def epsilon[A]: Epsilon[A] = Epsilon[A]()

  implicit def tuple2Prod[A](tpl: (GrammarElement[A], Double)): Production[A] = 
    Production(tpl::Nil)
  implicit def tupleWord2Prod[A](tpl: (A, Double)): Production[A] = {
    val (word, weight) = tpl
    Production((Word(word), weight) :: Nil)
  }
  implicit def wordVal2WordElt[A, B<:A](word: B): Word[A] = Word(word)

}


/** trait used to regroup word and epilon as a common
  * case when generating sentences
  */
sealed trait Terminal[A] extends GrammarElement[A] with PrefixOperator {
  val arity = 0

  /*  terminal does not carry its weight
   * to overcome this, use implicit conversion from tuple
   * by default, weight 1.0 is used */
  def orComposition(that: =>GrammarElement[A], weight: Double): Production[A] =
    Production(List((this, 1.0), (that, weight)))

  def andComposition(that: =>GrammarElement[A]): Rule[A] = 
    Rule(List(this, that))

  val toName: String = toString

  def compareBody(that: GrammarElement[A]): Boolean = this equals that

}

/** Word represents the non-epsilon terminal values of the grammar
  * Usually called Character of an alphabet, it would be confusing
  * with the class Char
  */
case class Word[A](value: A) extends Terminal[A] {
  def rToString(callers: List[AnyRef]): String = value.toString

  def rNullable(callers: List[GrammarElement[A]]): Boolean = false
  def rFirsts(callers: List[GrammarElement[A]]): Set[A] = Set(value)
}

/** Epsilon terminal of grammar
  */
case class Epsilon[A]() extends Terminal[A] {
  def rToString(callers: List[AnyRef]): String = "'e'"

  def rNullable(callers: List[GrammarElement[A]]): Boolean = true
  def rFirsts(callers: List[GrammarElement[A]]): Set[A] = Set()
}

/** A Rule is a sequential list of grammar elements
  * Rule should not be recursive (induce non terminating grammars)
  * but this may be useful, depending on ending conditions
  * Use by name parameters to avoid null pointer when recursive rules
  */
class Rule[A](m_body: =>List[GrammarElement[A]]) extends GrammarElement[A] with PrefixOperator {
  lazy val arity = body count {
    case m:Message[_, _] => false
    case _ => true
  }

  lazy val body: List[GrammarElement[A]] = m_body
  lazy val id: String = "R_" + GEIdGen()

  lazy val toName: String = id
  def rToString(callers: List[AnyRef]): String = {
    body map { se =>
      if ( this :: callers contains(se)) se.toName
      else se.rToString(this::callers)
    } mkString (id + ":(", " ** ", ")")
  }

  /*  rule does not carry its weight
   * to overcome this, use implicit conversion from tuple
   * by default, weight 1.0 is used */
  def orComposition(that: =>GrammarElement[A], weight: Double): Production[A] =
    Production(List((this, 1.0), (that, weight)))
  def andComposition(that: =>GrammarElement[A]): Rule[A] =
    Rule(body :+ that)

  def compareBody(that: GrammarElement[A]): Boolean = that match {
    case Rule(thatBody) =>
      if (thatBody.size == body.size) (body zip thatBody) forall { case (a, b) =>
        a compareBody b
      } else false
    case _ => false
  }

  def rNullable(callers: List[GrammarElement[A]]): Boolean = {
    def inCallers(se: GrammarElement[A]): Boolean = this :: callers contains se
    body forall { e => !inCallers(e) && e.rNullable(this :: callers) }
  }

  def rFirsts(callers: List[GrammarElement[A]]): Set[A] = {
    def inCallers(se: GrammarElement[A]): Boolean = this :: callers contains se
    def prefix = body span (_.nullable) match {
      case (prefix, head :: tail) => prefix :+ head
      case (prefix, Nil) => prefix
    }
    prefix.toSet filterNot { inCallers(_) } flatMap {
      _ rFirsts (this :: callers)
    }
  }
}

object Rule {
  def apply[A](body: =>List[GrammarElement[A]]): Rule[A] = new Rule(body)
  def apply[A](body: A*): Rule[A] =
    new Rule((body map Word[A]).toList)
  def unapply[A](rule: Rule[A]): Option[List[GrammarElement[A]]] = Some(rule.body)
}


/** A production is a list of possible weighted rules. one has to be chosen
  * in order to generate with the grammar
  */
class Production[A](m_body: =>List[(GrammarElement[A], Double)]) extends GrammarElement[A] {
  lazy val body: List[(GrammarElement[A], Double)] = m_body
  lazy val id: String = "P_" + GEIdGen()

  lazy val toName: String = id
  def rToString(callers: List[AnyRef]): String = body map { case (se, w) =>
    if (this :: callers contains se) "(" + se.toName + ", " + w + ")"
    else "(" + se.rToString(this::callers) + ", " + w + ")"
  } mkString (id + ":(", " || ", ")") 

  def orComposition(that: =>GrammarElement[A], weight: Double): Production[A] =
    Production(body :+ (that, weight))
  def andComposition(that: =>GrammarElement[A]): Rule[A] =
    Rule(List(this, that))

  def compareBody(that: GrammarElement[A]): Boolean = that match {
    case Production(thatBody) =>
      if (body.size == thatBody.size) (body zip thatBody) forall {
        case ((ae, aw), (be, bw)) => (aw == bw) && (ae compareBody be)
      } else false
    case _ => false
  }

  def rNullable(callers: List[GrammarElement[A]]): Boolean = {
    def inCallers(se: GrammarElement[A]): Boolean = this :: callers contains se
    body exists { case (e, w) =>
      w > 0 && !inCallers(e) &&  e.rNullable(this :: callers) }
  }

  def rFirsts(callers: List[GrammarElement[A]]): Set[A] = {
    val valid: PartialFunction[(GrammarElement[A], Double),GrammarElement[A]] = {
      case (e, w) if w > 0 && !(this :: callers contains e) => e
    }

    body.toSet collect valid flatMap { _ rFirsts (this :: callers) }
  }
}

object Production {
  def apply[A](body: =>List[(GrammarElement[A], Double)]): Production[A] =
    new Production(body)
  def apply[A](words: (A, Double)*): Production[A] = new Production(
    words.toList map { case (wo, we) => (Word(wo), we) }
  )
  def unapply[A](production: Production[A]): Option[List[(GrammarElement[A], Double)]] =
    Some(production.body)
}



object GEIdGen {
  def reset: Unit = counter = 0
  private var counter: Int = 0
  def apply(): Int = {
    counter += 1
    counter
  }
}

/** message emmitted in grammar generating elements of type A,
  * addressed to grammar generating elements of type B.
  * A can be equals to B
  */
sealed abstract class Message[A, B](refinement: =>GrammarElement[B])
    extends GrammarElement[A] {
  lazy val message: GrammarElement[B] = refinement
  lazy val id: String = "M_" + GEIdGen()

  lazy val toName: String = id
  def rToString(callers: List[AnyRef]): String =
    if (callers contains message) id + ":(" + message.toName + ")"
    else id + ":(" + message.rToString(message :: callers) + ")"
  override lazy val toString: String = rToString(this::Nil)

   /*  terminal does not carry its weight
   * to overcome this, use implicit conversion from tuple
   * by default, weight 1.0 is used */
  def orComposition(that: =>GrammarElement[A], weight: Double): Production[A] =
    Production(List((this, 1.0), (that, weight)))

  def andComposition(that: =>GrammarElement[A]): Rule[A] = 
    Rule(List(this, that))


  // view of nullable and firsts is relative to direct production and not
  // refinement's behavior
  def rNullable(callers: List[grammar.GrammarElement[A]]): Boolean = true
  def rFirsts(callers: List[grammar.GrammarElement[A]]): Set[A] = Set()


}

class HarmRefine[A](refinement: =>GrammarElement[Chord])
    extends Message[A, Chord](refinement) {
  def compareBody(that: GrammarElement[A]): Boolean = that match {
    case HarmRefine(thatRefinement) => refinement compareBody thatRefinement
    case _ => false
  }
}

object HarmRefine {
  def apply[A](ge: =>GrammarElement[Chord]): HarmRefine[A] = 
    new HarmRefine[A](ge)
  def unapply[A](hr: HarmRefine[A]): Option[GrammarElement[Chord]] =
    Some(hr.message)
}


class RootRythmRefine[A](refinement: =>GrammarElement[BPM])
    extends Message[A, BPM](refinement) {
  def compareBody(that: GrammarElement[A]): Boolean = that match {
    case RootRythmRefine(thatRefinement) => refinement compareBody thatRefinement
    case _ => false
  }
}

object RootRythmRefine {
  def apply[A](ge: =>GrammarElement[BPM]): RootRythmRefine[A] = 
    new RootRythmRefine[A](ge)
  def unapply[A](rrr: RootRythmRefine[A]): Option[GrammarElement[BPM]] =
    Some(rrr.message)
}


class RythmRefine[A](refinement: =>GrammarElement[BPM])
    extends Message[A, BPM](refinement) {
  def compareBody(that: GrammarElement[A]): Boolean = that match {
    case RythmRefine(thatRefinement) => refinement compareBody thatRefinement
    case _ => false
  }
}

object RythmRefine {
  def apply[A](ge: =>GrammarElement[BPM]): RythmRefine[A] = 
    new RythmRefine[A](ge)
  def unapply[A](rr: RythmRefine[A]): Option[GrammarElement[BPM]] =
    Some(rr.message)
}


class MelodyRefine[A](refinement: =>GrammarElement[Tone])
    extends Message[A, Tone](refinement) {
  def compareBody(that: GrammarElement[A]): Boolean = that match {
    case MelodyRefine(thatRefinement) => refinement compareBody thatRefinement
    case _ => false
  }
}

object MelodyRefine {
  def apply[A](ge: =>GrammarElement[Tone]): MelodyRefine[A] = 
    new MelodyRefine[A](ge)
  def unapply[A](mr: MelodyRefine[A]): Option[GrammarElement[Tone]] =
    Some(mr.message)
}
