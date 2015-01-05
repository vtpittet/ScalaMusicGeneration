package grammar

sealed trait SimpleElement[A] {
  def orComposition(that: =>SimpleElement[A], weight: Double): SimpleProduction[A]
  def andComposition(that: =>SimpleElement[A]): SimpleRule[A]

  val toName: String
  def toString(callers: List[SimpleElement[A]]): String

  def |(that: =>SimpleElement[A], weight: Double): SimpleProduction[A] =
    orComposition(that, weight)
  def |(word: A, weight: Double): SimpleProduction[A] =
    orComposition(SimpleWord(word), weight)
  def *(that: =>SimpleElement[A]): SimpleRule[A] =
    andComposition(that)
  def *(word: A): SimpleRule[A] =
    andComposition(SimpleWord(word))
}

object SimpleElement {
  def eps[A]: SimpleEpsilon[A] = SimpleEpsilon[A]()
  implicit def tuple2SimpleProd[A](tpl: (SimpleElement[A], Double)): SimpleProduction[A] =
    SimpleProduction(tpl::Nil)
}

sealed trait SimpleTerminal[A] extends SimpleElement[A] {
  /* Simple terminal does not carry its weight
   * to overcome this, use implicit conversion from tuple
   * by default, weight 1.0 is used */
  def orComposition(that: =>SimpleElement[A], weight: Double): SimpleProduction[A] =
    SimpleProduction(List((this, 1.0), (that, weight)))

  def andComposition(that: =>SimpleElement[A]): SimpleRule[A] = 
    SimpleRule(List(this, that))

  val toName: String = toString
  def toString(callers: List[SimpleElement[A]]): String = toString
}

case class SimpleWord[A](value: A) extends SimpleTerminal[A] {
  override val toString: String = value.toString
}

case class SimpleEpsilon[A]() extends SimpleTerminal[A] {
  override val toString: String = "'e'"
}


/** SimpleRule should not be recursive (induce non terminating grammars)
  * but this may be useful, depending on ending conditions
  * Use by name parameters to avoid null pointer when recursive rules
  */
class SimpleRule[A](m_body: =>List[SimpleElement[A]]) extends SimpleElement[A] {
  lazy val body: List[SimpleElement[A]] = m_body
  val id: String = "R_" + SGIdGen()

  val toName: String = id
  def toString(callers: List[SimpleElement[A]]): String = body map { se =>
    if (callers contains se) se.toName
    else se.toString(this::callers)
  } mkString (id + ":(", " * ", ")")
  override lazy val toString: String = toString(this::Nil)

  /* Simple rule does not carry its weight
   * to overcome this, use implicit conversion from tuple
   * by default, weight 1.0 is used */
  def orComposition(that: =>SimpleElement[A], weight: Double): SimpleProduction[A] =
    SimpleProduction(List((this, 1.0), (that, weight)))
  def andComposition(that: =>SimpleElement[A]): SimpleRule[A] =
    SimpleRule(body :+ that)
}

object SimpleRule {
  def apply[A](body: =>List[SimpleElement[A]]): SimpleRule[A] = new SimpleRule(body)
  def unapply[A](rule: SimpleRule[A]): Option[List[SimpleElement[A]]] = Some(rule.body)
}

class SimpleProduction[A](m_body: =>List[(SimpleElement[A], Double)]) extends SimpleElement[A] {
  lazy val body = m_body
  val id: String = "P_" + SGIdGen()

  val toName: String = id
  def toString(callers: List[SimpleElement[A]]): String = body map { case (se, w) =>
    if (callers contains se) "(" + se.toName + ", " + w + ")"
    else "(" + se.toString(this::callers) + ", " + w + ")"
  } mkString (id + ":(", " | ", ")") 
  override lazy val toString: String = toString(this::Nil)

  def orComposition(that: =>SimpleElement[A], weight: Double): SimpleProduction[A] =
    SimpleProduction(body :+ ((that, weight)))
  def andComposition(that: =>SimpleElement[A]): SimpleRule[A] =
    SimpleRule(List(this, that))
}

object SimpleProduction {
  def apply[A](body: =>List[(SimpleElement[A], Double)]): SimpleProduction[A] = new SimpleProduction(body)
  def unapply[A](production: SimpleProduction[A]): Option[List[(SimpleElement[A], Double)]] =
    Some(production.body)
}

object SGIdGen {
  private var counter: Int = 0
  def apply(): Int = {
    counter += 1
    counter
  }
}
