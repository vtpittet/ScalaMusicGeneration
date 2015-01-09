package generation
import chord.Chord
import rythmics.BPM
import rythmics.RythmCell
import tonalSystem.Tone
import grammar._
import generation.StackTask._


trait PartialSolution[+A <: PartialSolution[A]] {
  val h: ParsingTree[Chord]
  val rr: ParsingTree[BPM]
  val rc: ParsingTree[RythmCell]
  val m: ParsingTree[Tone]

  type Next <: PartialSolution[Next]
  type Current <: ParsingTree[_]

  val toNext: Next
  val current: Current

  def updateCurrentProb(newProb: Double): A
  def updateProbs(ph: Double, prr: Double, prc: Double, pm: Double): A

  // generates one step of 'current' tree and go to next.
  // Consider context constraint but not messages (or maybe yes, melody)
  def gen: List[Next]

  // easy test if this partial solution is a good solution
  lazy val completed: Boolean = h.isClosed //&& rr.isClosed && rc.isClosed && m.isClosed

  // should not reach zero by roundings
  // prob > ParsingTree.tresholdProg ^ 4 (initially 0.01 ^ 4 => 10^-8)
  lazy val prob: Double = h.prob * rr.prob * rc.prob * m.prob

  /*
   * do not generate but take (and drop) emmitted messages and distribute
   * to correct tree as refinement
   */
  def dispatchMsgs: (ParsingTree[Chord], ParsingTree[BPM], ParsingTree[RythmCell], ParsingTree[Tone]) = {
    val allMsgs: List[Message[_, _]] =
      h.getMessages ++ rr.getMessages ++ rc.getMessages ++ m.getMessages
    
    def dropRefine[A](t: ParsingTree[A], r: List[StackTask[A]]): ParsingTree[A] = {
      val dt = t.dropMessages
      dt.updated(stack = r ++ dt.stack)
    }

    val hRefs: List[Refine[Chord]] = allMsgs collect {
      case HarmRefine(refinement) => Refine(refinement)
    }
    val newH: ParsingTree[Chord] = dropRefine(h, hRefs)

    val rrRefs: List[Refine[BPM]] = allMsgs collect {
      case RootRythmRefine(refinement) => Refine(refinement)
    }
    val newRR: ParsingTree[BPM] = dropRefine(rr, rrRefs)

    val rcRefs: List[Refine[RythmCell]] = allMsgs collect {
      case RythmRefine(refinement) => Refine(refinement)
    }
    val newRC: ParsingTree[RythmCell] = dropRefine(rc, rcRefs)

    val mRefs: List[Refine[Tone]] = allMsgs collect {
      case MelodyRefine(refinement) => Refine(refinement)
    }
    val newM: ParsingTree[Tone] = dropRefine(m, mRefs)

    (newH, newRR, newRC, newM)
  }

}

object PartialSolution {
  def normalize[A <: PartialSolution[A]](target: List[A]): List[A] = {
    val (th, trr, trc, tm) = ((0.0, 0.0, 0.0, 0.0) /: target) {
      case ((th, trr, trc, tm), ps) => (
        th + ps.h.prob,
        trr + ps.rr.prob,
        trc + ps.rc.prob,
        tm + ps.m.prob
      )
    }

    target map { _.updateProbs(th, trr, trc, tm) }
  }
}



case class Harm(
  h: ParsingTree[Chord],
  rr: ParsingTree[BPM],
  rc: ParsingTree[RythmCell],
  m: ParsingTree[Tone]
) extends PartialSolution[Harm] {
  type Next = Root
  type Current = ParsingTree[Chord]

  val current = h
  lazy val toNext = Root(h, rr, rc, m)


  def updateCurrentProb(newProb: Double): Harm = Harm(
    h.updated(prob = newProb),
    rr, rc, m
  )
  def updateProbs(ph: Double, prr: Double, prc: Double, pm: Double): Harm = Harm(
    h.updated(prob = ph),
    rr.updated(prob = prr),
    rc.updated(prob = prc),
    m.updated(prob = pm)
  )

  def gen: List[Next] = {
    // dispatched messages
    val dH: Harm = this.dispatchMsgs match {
      case (nh, nrr, nrc, nm) => Harm(nh, nrr, nrc, nm)
    }

    dH.genNoDispatch
  }

  def genNoDispatch: List[Next] = {
    val nextMelodyTones = m.nextWords

    def suitableChord(c: Chord): Boolean = {
      nextMelodyTones.isEmpty || (nextMelodyTones exists { c contains _ })
    }

    val nextH = h.nexts(suitableChord(_), close = false)

    nextH map { Root(_, rr, rc, m) }

  }
}

case class Root(
  h: ParsingTree[Chord],
  rr: ParsingTree[BPM],
  rc: ParsingTree[RythmCell],
  m: ParsingTree[Tone]
) extends PartialSolution[Root] {
  type Next = Cell
  type Current = ParsingTree[BPM]

  val current = rr
  lazy val toNext = Cell(h, rr, rc, m)

  def updateCurrentProb(newProb: Double): Root = Root(
    h,
    rr.updated(prob = newProb),
    rc, m
  )
  def updateProbs(ph: Double, prr: Double, prc: Double, pm: Double): Root = Root(
    h.updated(prob = ph),
    rr.updated(prob = prr),
    rc.updated(prob = prc),
    m.updated(prob = pm)
  )
  def gen: List[Next] = {
    rr.nexts(x => true, h.isClosed) map { Cell(h, _, rc, m) }
  }
}

case class Cell(
  h: ParsingTree[Chord],
  rr: ParsingTree[BPM],
  rc: ParsingTree[RythmCell],
  m: ParsingTree[Tone]
) extends PartialSolution[Cell] {
  type Next = Melody
  type Current = ParsingTree[RythmCell]

  val current = rc
  lazy val toNext = Melody(h, rr, rc, m)

  def updateCurrentProb(newProb: Double): Cell = Cell(
    h, rr,
    rc.updated(prob = newProb),
    m
  )

  def updateProbs(ph: Double, prr: Double, prc: Double, pm: Double): Cell = Cell(
    h.updated(prob = ph),
    rr.updated(prob = prr),
    rc.updated(prob = prc),
    m.updated(prob = pm)
  )

  def gen: List[Next] = {
    rc.nexts(x => true, close = h.isClosed) map { Melody(h, rr, _, m) }
  }
}

case class Melody(
  h: ParsingTree[Chord],
  rr: ParsingTree[BPM],
  rc: ParsingTree[RythmCell],
  m: ParsingTree[Tone]
) extends PartialSolution[Melody] {
  type Next = Harm
  type Current = ParsingTree[Tone]

  val current = m
  lazy val toNext = Harm(h, rr, rc, m)

  def updateCurrentProb(newProb: Double): Melody = Melody(
    h, rr, rc,
    m.updated(prob = newProb)
  )

  def updateProbs(ph: Double, prr: Double, prc: Double, pm: Double): Melody = Melody(
    h.updated(prob = ph),
    rr.updated(prob = prr),
    rc.updated(prob = prc),
    m.updated(prob = pm)
  )

  def gen: List[Next] = rc.lastWord match {
    case None => List(Harm(h, rr, rc, m))
    case Some(cell) if cell.size == 1 => genOne
    case Some(cell) => realGen(cell.size)
  }

  private def genOne: List[Next] = {
    val suitableFirstTone: Tone => Boolean = tone => h.lastWord match {
      case None => true
      case Some(chord) => chord contains tone
    }

    val probExtractor: ParsingTree[Tone] => Double = _.prob

    val gens = m.nexts(suitableFirstTone, close = h.isClosed)

    gens map (Harm(h, rr, rc, _))
  }

  private def realGen(count: Int): List[Next] = {

//    println("realgen with count" + count)

    val suitableFirstTone: Tone => Boolean = h.lastWord match {
      case None => x => true
      case Some(chord) => chord contains _
    }

    val firstGen = m.nexts(suitableFirstTone, close = false)

    val generators: List[ParsingTree[Tone] => List[ParsingTree[Tone]]] =
      List.fill(count-2)(_.nexts(x => true, close = false))

    val lastGen: ParsingTree[Tone] => List[ParsingTree[Tone]] =
      _.nexts(x => true, close = h.isClosed)

    val probExtractor: ParsingTree[Tone] => Double = _.prob

    def probUpdator(t: ParsingTree[Tone], p: Double): ParsingTree[Tone] =
      t.updated(prob = p)



    val gens = ParsingTree.boundMultiGen[ParsingTree[Tone]](
      firstGen, generators :+ lastGen, probExtractor, probUpdator(_, _)
    )

    gens map (Harm(h, rr, rc, _))
  }
}
