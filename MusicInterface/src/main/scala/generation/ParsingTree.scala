package generation

import grammar._

sealed trait ParsingTree[A] { self =>
  val rTree: List[PrefixOperator with GrammarElement[A]]
  val prob: Double
  val msgs: List[Message]

  lazy val wordSize: Int = rTree count {
    case Word(_) => true
    case _ => false
  }

  lazy val lastWord: Option[A] = rTree collectFirst { case Word(w) => w }

  /*
   * Close trees accept everything, closed trees are accepted everywhere
   * Open trees accpet only open trees with same lastWord optional
   * Open trees with no lastword accept open trees with no lastword.
   */
  def accept(that: ParsingTree[A]): Option[ParsingTree[A]] = self match {
    // closed (main) Tree will not care anymore about refinements
    // approach here: main grammar may finish even if secondary not
    case ClosedTree(rT, p, m) => Some(that)
    case OpenTree(rT, s, p, r, m) => that match {
      case ClosedTree(rT, p, m) => Some(that)
      case _ => if (self.lastWord == that.lastWord) Some(that) else None
    }
  }

  def accept(thats: List[ParsingTree[A]]): List[ParsingTree[A]] =
    thats flatMap { self accept _ }
}


case class ClosedTree[A](
  rTree: List[PrefixOperator with GrammarElement[A]],
  prob: Double,
  msgs: List[Message])
    extends ParsingTree[A]


case class OpenTree[A](
  rTree: List[PrefixOperator with GrammarElement[A]],
  stack: List[Todo[A]],
  prob: Double,
  refs: List[OpenTree[A]],
  msgs: List[Message])
    extends ParsingTree[A] { self =>
  import ParsingTree._

  def oneStackStep: List[ParsingTree[A]] = stack match {
    case Nil => List(self.close)
    case Generate(g) :: stack => generate(g, stack)
    case Refine(g) :: stack =>
      // Indicates failure of applying this refinement
      if (refs.size >= maxRefinements) Nil
      else List(OpenTree(rTree, stack, prob, OpenTree(g)::refs, msgs))
    case (m: Message) :: stack  =>
      List(OpenTree(rTree, stack, prob, refs, m :: msgs))
  }

  /*
   * contract: next will either return open trees t' with :
   *   t.wordSize + 1 = t'.wordSize
   * or closed trees t'' with:
   *   t.wordSize = t'.wordSize + 1
   * 
   * Additionnaly, the size of returned list is limited to avoid complexity
   */
  def nexts: List[ParsingTree[A]] = {
    // find nexts for this tree
    // find nexts for each refinement trees
    // result is a ~join of lists on generated trees
    // join operation : closed tree match to any open tree
    // (special behavior closed left or right hand side)
    // join operation will limit breadth using probabilities
    self :: Nil
  }

  private def nextsMain: List[ParsingTree[A]] = {
    // limit with prob and not normalize here v
    val list = self.oneStackStep
    val pending: List[OpenTree[A]] = list collect {
      case that: OpenTree[A] if that.wordSize + 1 == self.wordSize => that
    }
    val generated: List[ParsingTree[A]] = list filterNot { pending contains _ }


    // and add loop detection : compare next gen rule and probabilities
    generated ::: (pending flatMap { _.nextsMain })

  }

  /** Returns all ParsingTrees with head of refinements evaluated,
    * accepted (else generated terminal equals last word of this or
    * refinements finishes) and put at the end of the refs list.
    * generated messages are lifted up to refined tree
    */
  private def refineHead: List[ParsingTree[A]] = refs match {
    case Nil => List(self)
    case h::t => normalize(self accept h.nexts) map {
      case ClosedTree(rT, p, m) =>
        self.updated(prob = prob * p, refs = t, msgs = m ::: msgs)
      case oT @ OpenTree(rT, s, p, r, m) =>
        self.updated(
          prob = prob * p,
          refs = t :+ oT.updated(msgs = Nil),
          msgs = oT.msgs ::: msgs)
    }
  }

  // builds a new open tree with updated specified values
  private def updated(
    rTree: List[PrefixOperator with GrammarElement[A]] = rTree,
    stack: List[Todo[A]] = stack,
    prob: Double = prob,
    refs: List[OpenTree[A]] = refs,
    msgs: List[Message] = msgs
  ): OpenTree[A] = OpenTree(rTree, stack, prob, refs, msgs)
/*
  def normalize(rules: List[(Rule[A], Double)]): List[(Rule[A], Double)] = {
    val total = rules.foldLeft(0.0)(_ + _._2)
    rules map { r => (r._1, r._2/total) }
  }
 */

  private def generate(
    g: GrammarElement[A],
    stack: List[Todo[A]],
    p: Double = 1): List[ParsingTree[A]] = g match {

    case t: Terminal[A] =>
      List(self.updated(rTree = t::rTree, stack = stack, prob = prob*p))
    case r @ Rule(body) =>
      List(self.updated(rTree = r :: rTree, stack = body ::: stack, prob = prob*p))
    case Production(rules) =>
      normalize(rules) { _._2 } { (x, y) => (x._1, y) } flatMap { r =>
        val (rule, pp) = r
        generate(rule, stack, p*pp)
      }
  }

  def close: ClosedTree[A] = ClosedTree(rTree, prob, msgs)

}

object OpenTree {
  def apply[A](ge: GrammarElement[A]): OpenTree[A] = {
    OpenTree(Nil, Generate(ge)::Nil, 1, Nil, Nil)
  }
}

object ParsingTree {
  val tresholdProb = 0.1
  val maxRefinements = 2

  def normalize[A](target: List[A])(extract: A => Double)(update: (A, Double) => A): List[A] = {
    val total = target.foldLeft(0.0) { _ + extract(_) }
    target map { t => update(t, extract(t)/total) }
  }

  def normalize[A](target: List[ParsingTree[A]]): List[ParsingTree[A]] =
    normalize[ParsingTree[A]](target) { _.prob } {
      case (ClosedTree(rT, p1, m), p2) => ClosedTree(rT, p2, m)
      case (OpenTree(rT, s, p1, r, m), p2) => OpenTree(rT, s, p2, r, m)
    }
/*
  def join[A](
    lhs: List[ParsingTree[A]],
    rhs: List[ParsingTree[A]]): List[ParsingTree[A]] = {

  }
 */
}
