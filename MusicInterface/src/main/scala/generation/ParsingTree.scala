package generation

import grammar._

case class ParsingTree[A](
  rTree: List[PrefixOperator],
  stack: List[Todo[A]],
  prob: Double,
  refs: List[ParsingTree[A]],
  msgs: List[Message]) { self =>

  def oneStackStep: List[ParsingTree[A]] = stack match {
    case Nil => self :: Nil
    case Generate(g) :: stack => generate(g, stack)
    case Refine(g) :: stack => ??? // TODO implement parallel refinement
    case (m: Message) :: stack  =>
      ParsingTree(rTree, stack, prob, refs, m :: msgs) :: Nil
  }

  private def generate(
    g: GrammarElement[A],
    stack: List[Todo[A]],
    p: Double = 1): List[ParsingTree[A]] = g match {

    case t: Terminal[A] => ParsingTree(t::rTree, stack, prob*p, refs, msgs) :: Nil
    case r @ Rule(body) =>
      ParsingTree(r :: rTree, (body map Generate[A]) ::: stack, prob*p, refs, msgs) :: Nil
    case Production(rules) => normalize(rules) flatMap { r =>
      val (rule, pp) = r
      generate(rule, stack, p*pp)
    }
  }

  def normalize(rules: List[(Rule[A], Double)]): List[(Rule[A], Double)] = {
    val total = rules.foldLeft(0.0)(_ + _._2)
    rules map { r => (r._1, r._2/total) }
  }

}
