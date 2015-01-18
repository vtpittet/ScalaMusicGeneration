package generation

import grammar._
import scala.util.Random


// TODO check where are branches that cannot generate words ?
case class ParsingTree[A](
  rTree: List[PrefixOperator with GrammarElement[A]],
  stack: List[StackTask[A]],
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

  lazy val onlyClosable: Boolean = {
    // collect firsts of nullable head of stack (eliminating messages)
    val nxtThis: Set[A] = (stack collect StackTask.grammElt span { _.nullable } match {
      case (nlb, hd::tail) => nlb :+ hd
      case (nlb, Nil) => nlb
    }).toSet flatMap { ge: GrammarElement[A] => ge.firsts }

    nxtThis.isEmpty && self.nullable
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
    val nxtThis: Set[A] = (stack collect StackTask.grammElt span { _.nullable } match {
      case (nlb, hd::tail) => nlb :+ hd
      case (nlb, Nil) => nlb
    }).toSet flatMap { ge: GrammarElement[A] => ge.firsts }

    // compute intersection with refinements
    (nxtThis /: refs) { _ intersect _.nextWords }
  }

  lazy val inNextWords: A => Boolean = nextWords contains _

  /** nextWords of this as if e was added at top of stack
    */
  def nextWordsWith(e: GrammarElement[A]): Set[A] =
    if (e.nullable) e.firsts union nextWords
    else e.firsts


  lazy val getMessages: List[Message[A, _]] =
    (refs flatMap (_.getMessages)) ++ msgs

  lazy val dropMessages: ParsingTree[A] = self.updated(
    msgs = Nil,
    refs = refs map (_.dropMessages)
  )

  lazy val collectWords: List[A] = rTree.collect{case Word(w) => w}.reverse

  /** returns a list of parsing trees such that each one is
    * one word bigger and in a state where there is one single deterministic
    * step to generate a new word or closed.
    * Note that this function should not close a tree without generating a word.
    * For the first call, aka instantiation of parsing tree from grammar
    * the function prepareNexts should be called
    * 
    * 
    * close and continue will select if after this nexts generation, the tree
    * must go to a state to continue or end the generation. (both can be true
    * if both acceptable)
    */
  def nexts(wishWord: A => Boolean, close: Boolean, continue: Boolean):
      List[ParsingTree[A]] = {
    val words = composeAnd(wishWord, inNextWords)
    //println(nextWords filter words)

    val gens = genNextWord(words)
    /*
    println("gens "+ gens.size)
    for(g <- gens) {
      println(g.collectWords)
      println(g.nextWords)
      println(stack)
      println(g.nullable)
      println(close)
    }
     */
    val prepared = gens flatMap (_.prepareGen(x => true, close, continue))


//    println("prep " + prepared.size)

    val res = normalize(
      elect(
        prepared
      ) { _.prob }
    )

//    println("res " + res.size)
    res
  }

  /** generate one-step word from prepared state and adjust refinements
    * may be Nil if no refinement can generate the next word
    * may be longer if refinement has more than one way to generate
    * refinement
    * 
    * The self tree must be prepared to generate (otherwise: message shift)
    * 
    * Result Must be limited
    * ref grammars must be updated
    * 
    * sanity check: is it satisfiable ?
    * first: prepare grammars: Grammar must be in state ready to gen
    * a terminal
    */
  def genNextWord(wishWord: A => Boolean): List[ParsingTree[A]] = {
    val words = composeAnd(wishWord, inNextWords)

    val size = self.wordSize

    def rejectTree(t: ParsingTree[A]): Boolean = {
      (t.nextWords filter words).isEmpty
    }

    // accept tree if one word generated (should accept as soon as word generated
    def acceptTree(t: ParsingTree[A]): Boolean = t.wordSize == size + 1

    def acceptChild(e: GrammarElement[A], t: ParsingTree[A]): Boolean = {
      (t.nextWordsWith(e) filter words).nonEmpty
    }

    // generator generates only acceptable Trees
    def generator(t: ParsingTree[A]): List[ParsingTree[A]] = {
      t.gen(rejectTree, acceptTree, acceptChild)
    }

    val mainGenerated: List[ParsingTree[A]] = rNexts(
      List(self),
      acceptTree(_),
      generator(_)
    )
    
    elect(mainGenerated flatMap (_.refNextWord(words)))(_.probWithRefHead)
  }


  def refHeadNextWord(wishWord: A => Boolean): List[ParsingTree[A]] = {

    // fix this word computation was wrong
    // called in already next word generated main tree
    // refinement will be generated
    val words = wishWord//composeAnd(wishWord, inNextWords)

    def generate(t: ParsingTree[A]): List[ParsingTree[A]] = {
      t.genNextWord(words)
    }

    genHeadRef(generate(_))
  }


  def refNextWord(wishWord: A => Boolean): List[ParsingTree[A]] = {

    // fixme this was wrong, intersection of already generated main tree with not yet generated ref tree
    val words = wishWord //composeAnd(wishWord, inNextWords)

    val generators: List[ParsingTree[A] => List[ParsingTree[A]]] =
      List.fill(refs.size)(_.refHeadNextWord(words))

    def probExtractor(t: ParsingTree[A]): Double = t.probWithRefHead

    def probUpdator(t: ParsingTree[A], p: Double): ParsingTree[A] =
      t.updateProbWithRefHead(p)

    boundMultiGen[ParsingTree[A]](List(self), generators, probExtractor(_), probUpdator(_, _))
  }

  /** Method called to put the tree where it is about to generate a
    * single terminal in a deterministic way
    * new approach : this is the key point. At each generation step,
    * the tree, additionnaly to the word generation will continue following
    * rules as far as possible, while no new word is generated and the
    * tree is still open
    * To ease the computation, don't forget to filter branches using firsts 
    *
    * close and continue indicate if the tree can be closed without generating
    * more words or respectively if it can generate another word (matching
    * wishWord predicate, ofc)
    * 
    */
  def prepareGen(wishWord: A => Boolean, close: Boolean, continue: Boolean): List[ParsingTree[A]] = {

    val words = composeAnd(wishWord, inNextWords)


    /*
     * 
     */
    def rejectTree(t: ParsingTree[A]): Boolean = {
      ((close && continue) && (!t.nullable && (t.nextWords filter words).isEmpty)) ||
      ((close && !continue) && !t.nullable) ||
      ((!close && continue) && (t.nextWords filter words).isEmpty) ||
      (!close && !continue)
    }
    
    /* without close, continue
    def rejectTree(t: ParsingTree[A]): Boolean = {
      !t.nullable && (t.nextWords filter words).isEmpty
    }
    */

    // accept tree if ready to generate word of wishWord and continue
    // or closed (stack is empty) and close
    def acceptTree(t: ParsingTree[A]): Boolean = t.stack match {
      case Nil => close
      case Word(w) :: tail => continue &&  words(w)
      case _ => false
    }

    def acceptChild(e: GrammarElement[A], t: ParsingTree[A]): Boolean = {
      (close && e.nullable && t.nullable) ||
      (continue && (t.nextWordsWith(e) filter words).nonEmpty)
    }

    // generator generates only acceptable Childrens
    // returned list contains accepted trees and pending solutions
    // (need to elect, normalize before generating further)
    def generator(t: ParsingTree[A]): List[ParsingTree[A]] = {
      t.gen(rejectTree, acceptTree, acceptChild)
    }


    val mainReady: List[ParsingTree[A]] = rNexts(
      List(self),
      acceptTree(_),
      generator(_)
    )

    elect(mainReady flatMap (_.prepareRefs(words)))(_.probWithRefHead)
  }


  /** generates next step of search tree : List composed of parsing trees
    * that are accepted (solutions) and parsing tree about to make a non
    * deterministic step
    * refs grammars must be updated as well !
    */
  def gen(
    rejectTree: ParsingTree[A] => Boolean, // security
    acceptTree: ParsingTree[A] => Boolean,
    acceptChild: (GrammarElement[A], ParsingTree[A]) => Boolean
  ): List[ParsingTree[A]] =
    if (acceptTree(self)) List(self)
    else if (rejectTree(self)) Nil
    else stack match {
      case Nil => Nil // not accepted and nothing to change

      case Refine(g) :: stk => if (refs.size >= maxRefinements) Nil else {
        self.updated(
          stack = stk,
          refs = ParsingTree(g)::refs
        ).gen(rejectTree, acceptTree, acceptChild)
      }

      case (m: Message[A, _]) :: stk  =>
        self.updated(
          stack = stk,
          msgs = m::msgs
        ).gen(rejectTree, acceptTree, acceptChild)

      case (t: Terminal[A]) :: stk =>
        self.updated(
          rTree = t::rTree,
          stack = stk
        ).gen(rejectTree, acceptTree, acceptChild)

      case (r @ Rule(body)) :: stk =>
        self.updated(
          rTree = r :: rTree,
          stack = body ::: stk
        ).gen(rejectTree, acceptTree, acceptChild)

      case Production(rules) :: stk => {
        /* filter out unviable childrens (typically cannot generate expected words)
         * normalize weights to 1 for probability computation
         */
        val normRules =
          normalize(rules filter { x => acceptChild(x._1, self) }) { _._2 } { (x, y) => (x._1, y) }

        /* create childrens with probability relative to parent and filter out
         * unprobable ones
         * to generate a child, 1) take an option of production 2) add it at top of stack 3) update prob
         */
        val childrens = normRules collect {
          case (ge, p) if (p*prob >= tresholdProb) =>
            self.updated(
              stack = ge::stk,
              prob = prob * p)
        }
        
        childrens flatMap {
          /* adding t.topStkIsProd to accept filter will cause recursion
           * to stop just before generating a production again
           */
          // TODO check contract of function, make sure that usage corresponds !!
          _.gen(rejectTree, t => acceptTree(t) || t.topStkIsProd, acceptChild)
        }
      }
    }


  def prepareHeadRef(wishWord: A => Boolean): List[ParsingTree[A]] = {

    val words = composeAnd(wishWord, inNextWords)

    def generate(t: ParsingTree[A]): List[ParsingTree[A]] = {
      // ref grammars can close and continue
      t.prepareGen(words, true, true)
    }

    genHeadRef(generate(_))
  }

  /** take all ref grammars and put them in a state where they are
    * ready to generate a new terminal in wishWord
    * (typically, nextWords of refined tree)
    */
  def prepareRefs(wishWord: A => Boolean): List[ParsingTree[A]] = if (self.isClosed) {
    List(self)
  } else {
    val words = composeAnd(wishWord, inNextWords)
    
    // note that wishWord are systematically intersected with self.nextWords
    val generators: List[ParsingTree[A] => List[ParsingTree[A]]] =
      List.fill(refs.size)(_.prepareHeadRef(words))
    def probExtractor(t: ParsingTree[A]): Double = t.probWithRefHead

    def probUpdator(t: ParsingTree[A], p: Double): ParsingTree[A] =
      t.updateProbWithRefHead(p)

    boundMultiGen[ParsingTree[A]](List(self), generators, probExtractor(_), probUpdator(_, _))
  }

    /** applies a generate method to head of refinements and
    * return self.updated with each new refinement at tail of refs list
    * (for the sake of cyclic use of genHeadRef). If generated grammar is
    * closed, then do not reuse that gramar
    * 
    * result of generate method must be
    * normalized in probability
    * all childrens of generated refinement must respect
    * nullable || (nextWords intersect wishWord /*of parent*/).nonEmpty
    * 
    * 
    */
  def genHeadRef(
    generate: ParsingTree[A] => List[ParsingTree[A]]
  ): List[ParsingTree[A]] = refs match {
    case Nil => List(self)
    case r :: rs => generate(r) map {
      case newRef if newRef.isClosed => self.updated(refs = rs)
      case newRef /* !newRef.isClosed */ => self.updated(refs = rs :+ newRef)
    }
  }


  // builds a new open tree with updated specified values
  def updated(
    rTree: List[PrefixOperator with GrammarElement[A]] = rTree,
    stack: List[StackTask[A]] = stack,
    prob: Double = prob,
    refs: List[ParsingTree[A]] = refs,
    msgs: List[Message[A, _]] = msgs
  ): ParsingTree[A] = ParsingTree(rTree, stack, prob, refs, msgs)

  private lazy val topStkIsProd: Boolean = stack match {
    case Production(_) :: tail => true
    case _ => false
  }

  private lazy val probWithRefHead: Double = refs match {
    case Nil => self.prob
    case r :: rs => self.prob * r.prob
  }

  def updateProbWithRefHead(p: Double): ParsingTree[A] = refs match {
    case Nil => self.updated(prob = p)
    case r :: rs => {
      val probFactor = math.sqrt(p/(self.prob*r.prob))

      val probSelf = self.prob * probFactor
      val probRef = r.prob * probFactor

      self.updated(refs = r.updated(prob = probRef) :: rs, prob = probSelf)
    }
  }
}

object ParsingTree {
  val maxRefinements = 2
  val maxMemory = 100
  val tresholdProb = math.pow(maxMemory, -3)

  def composeOr[A](lhs: A => Boolean, rhs: A => Boolean): A => Boolean = { a =>
    lhs(a) || rhs(a)
  }

  def composeAnd[A](lhs: A => Boolean, rhs: A => Boolean): A => Boolean = { a =>
    lhs(a) && rhs(a)
  }

  def compose[A](lhs: A => Boolean, rhs: A => Boolean)(
    combine: (Boolean, Boolean) => Boolean): A => Boolean = { a =>
    lhs(a) || rhs(a)
  }

  val truePrd: Any => Boolean = x => true


  def normalize[A](target: List[A])(extract: A => Double)(update: (A, Double) => A): List[A] = {
    val total = (0.0 /: target) { _ + extract(_) }
    if (total <= 0 && target != Nil) {
      // TODO remove debug output
      println("[warn]: normalizing with total <= 0")
      Nil
    } else target map { t => update(t, extract(t)/total) }
  }


  def normalize[A](target: List[ParsingTree[A]]): List[ParsingTree[A]] =
    normalize[ParsingTree[A]](target) { _.prob } {
      case (tree, p2) => tree.updated(prob = p2)
    }

  def limit[A](target: List[A])(extract: A => Double): List[A] = {
    elect(target)(extract)
  }

  def elect[A](target: List[A])(extract: A => Double): List[A] =
    elect(maxMemory, target)(extract)

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

  // must test that target is not null
  def chooseOne[A](target: List[A])(extract: A => Double): A = {
    // Error possible from here if targe == Nil
    electOne(target)(extract)._1.get
  }

  /** applies The algorithm.
    * Solutions is partitionned in valid solutions and pending solutions.
    * if pending is empty, return
    * pending solutions are flat mapped with generator => ps
    * valid solutions and ps are limited, then recursion
    */
  def rNexts[A](
    solutions: List[ParsingTree[A]],
    acceptTree: ParsingTree[A] => Boolean,
    generator: ParsingTree[A] => List[ParsingTree[A]]
  ): List[ParsingTree[A]] = solutions partition acceptTree match {
    case (sol, Nil) => sol
    case (sol, pen) => {
      val candidates = sol ::: (pen flatMap generator)
      rNexts(
        elect(candidates) { _.prob },
        acceptTree, generator
      )
    }

  }

  /** bounded multiple generations is similar to Iterator.iterate
    * a list of generation methods are provided, a probability extractor
    * and an initial list of solutions
    * while generators are not empty, generate for all solutions,
    * limit using probability extractor and recurse on tail of generators
    */

  def boundMultiGen[A](
    solutions: List[A],
    generators: List[A => List[A]],
    probExtractor: A => Double,
    probUpdator: (A, Double) => A
  ): List[A] = generators match {
    case Nil => solutions
    case gen :: gens => boundMultiGen(
      normalize(
        elect(solutions flatMap gen)(probExtractor)
      )(probExtractor)(probUpdator),
      gens, probExtractor, probUpdator)
  }


  def apply[A](ge: GrammarElement[A]): ParsingTree[A] = {
    ParsingTree(Nil, ge::Nil, 1, Nil, Nil)
  }

}
