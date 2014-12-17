package generation

import grammar._

sealed trait ParsingTree[A] {
  val rTree: List[PrefixOperator]
  val prob: Double
}


case class ClosedTree[A](
  rTree: List[PrefixOperator],
  prob: Double)
    extends ParsingTree[A]


case class OpenTree[A](
  rTree: List[PrefixOperator],
  stack: List[Todo[A]],
  prob: Double,
  refs: List[ParsingTree[A]],
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

  def nexts: List[ParsingTree[A]] = {
    // find nexts for this tree
    // find nexts for each refinement trees
    // result is a join of lists on generated trees
    self :: Nil
  }

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

    case t: Terminal[A] => OpenTree(t::rTree, stack, prob*p, refs, msgs) :: Nil
    case r @ Rule(body) =>
      OpenTree(r :: rTree, body ::: stack, prob*p, refs, msgs) :: Nil
    case Production(rules) =>
      normalize(rules) { _._2 } { (x, y) => (x._1, y) } flatMap { r =>
        val (rule, pp) = r
        generate(rule, stack, p*pp)
      }
  }

  def close: ClosedTree[A] = ClosedTree(rTree, prob)
}

object OpenTree {
  def apply[A](ge: GrammarElement[A]): ParsingTree[A] = {
    OpenTree(Nil, Generate(ge)::Nil, 1, Nil, Nil)
  }
}

object ParsingTree {
  val tresholdProb = 0.1
  val maxRefinements = 2
  def normalize[T](target: List[T])(extract: T => Double)(update: (T, Double) => T): List[T] = {
    val total = target.foldLeft(0.0) { _ + extract(_) }
    target map { t => update(t, extract(t)/total) }
  }

}
