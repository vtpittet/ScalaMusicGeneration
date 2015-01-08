package generation

import grammar._
import scala.util.Random

case class ParsingTree[A](
  rTree: List[PrefixOperator with GrammarElement[A]],
  stack: List[Task[A]],
  prob: Double,
  refs: List[ParsingTree[A]],
  msgs: List[Message[A, _]]) { self =>

  import ParsingTree._


  lazy val wordSize: Int = rTree count {
    case Word(_) => true
    case _ => false
  }

  lazy val lastWord: Option[A] = rTree collectFirst { case Word(w) => w }

  val isClosed: Boolean = stack.isEmpty

  /*
   * Closed trees accept everything, closed trees are accepted everywhere
   * Open trees accpet only open trees with same lastWord optional
   * Open trees with no lastword accept open trees with no lastword.
   */
  def accept(that: ParsingTree[A]): Option[ParsingTree[A]] =
    // closed (main) Tree will not care anymore about refinements
    // approach here: main grammar may finish even if secondary not
    if (self.isClosed ||
      that.isClosed ||
      (self.lastWord == that.lastWord)) Some(that)
    else None

  def accept(thats: List[ParsingTree[A]]): List[ParsingTree[A]] =
    thats flatMap { self accept _ }

  /** returns a list of parsing trees such that each one is
    * one word bigger and in a state where there is one single deterministic
    * step to generate a new word or closed.
    * Note that this function should not close a tree without generating a word.
    * For the first call, aka instantiation of parsing tree from grammar
    * the function prepareNexts should be called
    */
  def nexts(wishWords: Set[A], closable: Boolean): List[ParsingTree[A]] =
    ParsingTree.rNexts(self.genNextWord(wishWords), wishWords, closable)


  /** generate one-step word from prepared state and adjust refinements
    * may be Nil if no refinement can generate the next word
    * may be longer if refinement has more than one way to generate
    * refinement
    * 
    * Result Must be limited
    */
  def genNextWord(wishWords: Set[A]): List[ParsingTree[A]] = ???


  def candidatesNextWord: List[ParsingTree[A]] = ???


  def candidatesPrepare(wishWords: Set[A]): List[ParsingTree[A]] = ???





  def gen(
    acceptTree: ParsingTree[A] => Boolean,
    acceptChild: GrammarElement[A] => Boolean
  ): List[ParsingTree[A]] = if (acceptTree(self)) List(self) else stack match {
    case Nil => Nil // not accepted and nothing to change

    case Refine(g) :: stk => if (refs.size >= maxRefinements) Nil else {
      self.updated(stack = stk, refs = ParsingTree(g)::refs).gen(acceptTree, acceptChild)
    }

    case (m: Message[A, _]) :: stk  =>
      self.updated(stack = stk, msgs = m::msgs).gen(acceptTree, acceptChild)

    case (t: Terminal[A]) :: stk =>
      self.updated(rTree = t::rTree, stack = stk).gen(acceptTree, acceptChild)

    case (r @ Rule(body)) :: stk =>
      self.updated(rTree = r :: rTree, stack = body ::: stk).gen(acceptTree, acceptChild)

    case Production(rules) :: stk => {
      /* filter out unviable childrens (typically cannot generate expected words)
       * normalize weights to 1 for probability computation
       */
      val n = normalize(rules filter { x => acceptChild(x._1) }) { _._2 } { (x, y) => (x._1, y) }

      /* create childrens with probability relative to parent and filter out
       * unprobable ones
       */
      n collect { case (rule @ Rule(body), p) if (p*prob >= tresholdProb) =>
        self.updated(
          rTree = rule :: rTree,
          stack = body ::: stk,
          prob = prob*p
        )
      }
    }
  }



  /* closed indicates : no terminal generated, tree closed
   * Success indicates : one terminal generated
   * Pending indicates : The choices of list need more steps to decide
   * Failure indicates : the constraints cannot be satisfied
   */
  private def oneGrammStep(
    accept: GrammarElement[A] => Boolean
  ): List[ParsingTree[A]] = stack match {
    case Nil => List(self)

    case Refine(g) :: stk =>
      // Indicates failure of applying this refinement
      if (refs.size >= maxRefinements) Nil
      else self.updated(stack = stk, refs = ParsingTree(g)::refs).oneGrammStep(accept)

    case (m: Message[A, _]) :: stk  =>
      self.updated(stack = stk, msgs = m::msgs).oneGrammStep(accept)

    case (t: Terminal[A]) :: stk =>
      List(self.updated(rTree = t::rTree, stack = stk))

    case (r @ Rule(body)) :: stk =>
      List(self.updated(rTree = r :: rTree, stack = body ::: stk))

    case Production(rules) :: stk => {
      normalize(rules) { _._2 } { (x, y) => (x._1, y) } map { r =>
        val (rule @ Rule(body), p) = r
        self.updated(rTree = rule :: rTree, stack = body ::: stk, prob = prob*p)
      }
    }
  }

  private def prepareNext(
    accept: GrammarElement[A] => Boolean,
    tempSols: List[ParsingTree[A]]
  ): List[ParsingTree[A]] = stack match {
    case Nil => List(self)

    case Refine(g) :: stk =>
      // Indicates failure of applying this refinement
      if (refs.size >= maxRefinements) Nil
      else self.updated(stack = stk, refs = ParsingTree(g)::refs).prepareNext(accept, tempSols)

    case (m: Message[A, _]) :: stk =>
      self.updated(stack = stk, msgs = m::msgs).prepareNext(accept, tempSols)

    case _ => ???

  }

  /* new approach : this is the key point. At each generation step,
   * the tree, additionnaly to the word generation will continue following
   * rules as far as possible, while no new word is generated and the
   * tree is still open
   * To ease the computation, don't forget to filter branches using firsts 
   */
  private def prepareNextWord(wishWords: Set[A]): List[ParsingTree[A]] = {
    stack match {
      case Nil => List(self) // closed
      case Word(w) :: stk => List(self) // good state, do not generate
      case _ => ??? //oneStackStep flatMap
    }
  }

  /** returns true if this tree can be closed without generating any word
    */
  lazy val nullable: Boolean = stack forall {
    case ge: GrammarElement[A] => ge.nullable
    case _ => true
  }

  /** returns the set of all possible next words generated by this tree,
    * considering it's known refinements
    */
  lazy val nextWords: Set[A] = {
    // collect firsts of nullable head of stack (eliminating messages)
    val nxtThis: Set[A] = (stack collect Task.grammElt span { _.nullable } match {
      case (nlb, hd::tail) => nlb :+ hd
      case (nlb, Nil) => nlb
    }).toSet flatMap { ge: GrammarElement[A] => ge.firsts }

    // compute intersection with refinements
    (nxtThis /: refs) { _ intersect _.nextWords }
  }



  /* new approach : next word will return the list of trees that exactly
   * generated one word and did not generate further.
   * this will be useful during the first generation (btw, prepareNextWord
   * could be used when creating a tree), but it has to return a list because
   * refinement trees may have multiple solution to generate the one word of
   * the main tree
   * 
   * new approach : generate one word and get ready for next generation
   * generates as far as possible without another word
   * 
   * should return a list of possible nexts of bounded size
   * 
   */
  /*
   * contract: next will either return open trees t' with :
   *   t.wordSize + 1 = t'.wordSize
   * or closed trees t'' with:
   *   t.wordSize = t'.wordSize + 1
   * 
   * Additionnaly, the size of returned list is limited to avoid complexity

  def rNexts(solutions: List[Result[A]]): List[ParsingTree[A]] = {

    self :: Nil
  }
   */
  private def nextsMain: List[ParsingTree[A]] = ???/*self.oneGrammStep match {
    case Failure => Nil
    case Closed(tree) => List(tree)
    case Success(tree) => List(tree)
    case Pending(trees) => ??? /*limit(trees) flatMap _.oneGrammStep*/
  }*/

/*
  private def nextsMain_old: List[ParsingTree[A]] = {
    // limit with prob and not normalize here v
    val list = self.oneGrammStep
    val pending: List[OpenTree[A]] = list collect {
      case that: OpenTree[A] if that.wordSize + 1 == self.wordSize => that
    }
    val generated: List[ParsingTree[A]] = list filterNot { pending contains _ }

    // and add loop detection : compare next gen rule and probabilities
    generated ::: (pending flatMap { _.nextsMain })

  }
 */

  /** Returns all ParsingTrees with head of refinements evaluated,
    * accepted (else generated terminal equals last word of this or
    * refinements finishes) and put at the end of the refs list.
    * generated messages are lifted up to refined tree
    *
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
    */

  // builds a new open tree with updated specified values
  private def updated(
    rTree: List[PrefixOperator with GrammarElement[A]] = rTree,
    stack: List[Task[A]] = stack,
    prob: Double = prob,
    refs: List[ParsingTree[A]] = refs,
    msgs: List[Message[A, _]] = msgs
  ): ParsingTree[A] = ParsingTree(rTree, stack, prob, refs, msgs)

/*
  def normalize(rules: List[(Rule[A], Double)]): List[(Rule[A], Double)] = {
    val total = rules.foldLeft(0.0)(_ + _._2)
    rules map { r => (r._1, r._2/total) }
  }
 */

}

object ParsingTree {
  val tresholdProb = 0.01
  val maxRefinements = 2
  val maxMemory = 10

  def normalize[A](target: List[A])(extract: A => Double)(update: (A, Double) => A): List[A] = {
    val total = (0.0 /: target) { _ + extract(_) }
    if (total <= 0) Nil else target map { t => update(t, extract(t)/total) }
  }


  def normalize[A](target: List[ParsingTree[A]]): List[ParsingTree[A]] =
    normalize[ParsingTree[A]](target) { _.prob } {
      case (tree, p2) => tree.updated(prob = p2)
    }

  def limit[A](target: List[A])(extract: A => Double): List[A] = {
    elect(maxMemory, target)(extract)
  }

  def elect[A](count: Int, target: List[A])(extract: A => Double): List[A] = {
    Iterator.iterate[(List[A], List[A])]((Nil, target)) { case (result, target) =>
      val (elected, left) = electOne(target)(extract)
      (elected.toList ::: result, left)
    }.drop(count).next._1
  }

  def electOne[A](target: List[A])(extract: A => Double): (Option[A], List[A]) = {
    val total = target.foldLeft(0.0) { _ + extract(_) }
    val random = Random.nextDouble * total

    def r_elect(r: Double, l: List[A]): (Option[A], List[A]) = l match {
      case Nil => (None, Nil)
      case x :: xs => {
        val px = extract(x)
        // elects this one
        if (r < px) (Some(x), xs)
        // elects a following
        else {
          val (result, left) = r_elect(r - px, xs)
          (result, x :: left)
        }
      }
    }

    r_elect(random, target)
  }

  /* c: candidate

  def nextGen[A](c: OpenTree[A]): List[ParsingTree[A]] = {
    c.nextsOneStep filterNot { _ == Failure })
  }
   */

  /** applies The algorithm.
    * Solutions is splitted in valid solutions and pending solutions.
    * if pending is empty, return
    * pending solutions are flat mapped with gen => ps
    * valid solutions and ps are limited, then recursion
    */
  def rNexts[A](
    solutions: List[ParsingTree[A]],
    wishWords: Set[A],
    closable: Boolean
  ): List[ParsingTree[A]] = {
    def acceptTree(t: ParsingTree[A]): Boolean = t match {
      case _ => true
    }
    ???
  }


  def apply[A](ge: GrammarElement[A]): ParsingTree[A] = {
    ParsingTree(Nil, ge::Nil, 1, Nil, Nil)
  }

}


