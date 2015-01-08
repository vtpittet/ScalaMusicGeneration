package generation
import chord.Chord
import rythmics.BPM
import rythmics.RythmCell
import tonalSystem.Tone
import grammar._
import generation.StackTask._


trait PartialSolution {
  val h: ParsingTree[Chord]
  val rr: ParsingTree[BPM]
  val rc: ParsingTree[RythmCell]
  val m: ParsingTree[Tone]

  type Next <: PartialSolution
  type Current <: ParsingTree[_]

  val toNext: Next

  // generates one step of 'current' tree and go to next.
  // Consider context constraint but not messages (or maybe yes, melody)
  def gen: List[Next]

  lazy val completed: Boolean = h.isClosed //&& rr.isClosed && rc.isClosed && m.isClosed

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


case class Harm(
  h: ParsingTree[Chord],
  rr: ParsingTree[BPM],
  rc: ParsingTree[RythmCell],
  m: ParsingTree[Tone]
) extends PartialSolution {
  type Next = Root
  type Current = ParsingTree[Chord]
  lazy val toNext = Root(h, rr, rc, m)


  def gen: List[Next] = {
    // dispatched messages
    val dH: Harm = this.dispatchMsgs match {
      case (nh, nrr, nrc, nm) => Harm(nh, nrr, nrc, nm)
    }

    dH.genNoDispatch
  }

  def genNoDispatch: List[Next] = {
    val nextMelodyTones = m.nextWords
    
    def suitableChord(c: Chord): Boolean = nextMelodyTones exists { c contains _ }

    val nextH = h.nexts(suitableChord(_))

    nextH map { Root(_, rr, rc, m) }

  }
}

case class Root(
  h: ParsingTree[Chord],
  rr: ParsingTree[BPM],
  rc: ParsingTree[RythmCell],
  m: ParsingTree[Tone]
) extends PartialSolution {
  type Next = Cell
  type Current = ParsingTree[BPM]
  lazy val toNext = Cell(h, rr, rc, m)

  def gen: List[Next] = {
    rr.nexts(x => true) map { Cell(h, _, rc, m) }
  }
}

case class Cell(
  h: ParsingTree[Chord],
  rr: ParsingTree[BPM],
  rc: ParsingTree[RythmCell],
  m: ParsingTree[Tone]
) extends PartialSolution {
  type Next = Melody
  type Current = ParsingTree[RythmCell]
  lazy val toNext = Melody(h, rr, rc, m)

  def gen: List[Next] = {
    rc.nexts(x => true) map { Melody(h, rr, _, m) }
  }
}

case class Melody(
  h: ParsingTree[Chord],
  rr: ParsingTree[BPM],
  rc: ParsingTree[RythmCell],
  m: ParsingTree[Tone]
) extends PartialSolution {
  type Next = Harm
  type Current = ParsingTree[Tone]
  lazy val toNext = Harm(h, rr, rc, m)

  def gen: List[Next] = {
    val count = rc.lastWord match {
      case None => 0
      case Some(cell) => cell.size
    }

    if (count <= 0) List(Harm(h, rr, rc, m))
    else realGen(count)
  }

  private def realGen(count: Int): List[Next] = {

    val suitableFirstTone: Tone => Boolean = h.lastWord match {
      case None => x => true
      case Some(chord) => chord contains _
    }

    val firstGen = m.nexts(suitableFirstTone)

    val generators: List[ParsingTree[Tone] => List[ParsingTree[Tone]]] =
      List.fill(count-1)(_.nexts(x => true))

    val probExtractor: ParsingTree[Tone] => Double = _.prob

    val gens = ParsingTree.boundMultiGen(firstGen, generators, probExtractor)

    gens map (Harm(h, rr, rc, _))
  }
}
