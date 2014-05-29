package segmentSystem

import rythmics.BPM
import tonalSystem.Tone
import tonalSystem.Tone._
import utils.PrettyPrinter
import tonalSystem.Scale
import tonalSystem.Major
import tonalSystem.C

sealed trait MusicalSegment extends MusicalSegmentLike[MusicalSegment]{
  
  type TStream = Stream[PartialFunction[MusicalSegment, MusicalSegment]]
  
  
  val melody: List[MusicalSegment]
  
  
  lazy val height: Int = if(!melody.isEmpty) melody.maxBy(_.height).height + 1 else 0
  
  val parDepth: Int
  val seqDepth: Int
  val length: Double
  
  lazy val notes: List[Note] = melody.flatMap(_.notes)
  
  val parallelBuilder: List[MusicalSegment] => ParallelSegment = Parallel(_)
  val sequentialBuilder: List[MusicalSegment] => SequentialSegment = Sequential(_)
  
  
  def +(that: MusicalSegment): SequentialSegment
  def ++(that: MusicalSegment): SequentialSegment = sequentialBuilder(List(this, that))
  
  def |(that: MusicalSegment): ParallelSegment
  def ||(that: MusicalSegment): ParallelSegment = parallelBuilder(List(this, that))
  
  /*
   * Note that parallel multiplication is nearly idempotent as the same melody is replayed at the same time
   * def *(repetitions: Int): ParallelSegment = ParallelSegment(List.fill(repetitions)(this))
   */
  def *(repetitions: Int): SequentialSegment = sequentialBuilder(List.fill(repetitions)(this))
  
  // assume identity at the beginning of each definition
  def *+(apps: Int)(transf: (MusicalSegment) => MusicalSegment*): SequentialSegment = {
    val applications = Stream.continually(transf).flatten.take(apps)
    multiTransf(sequentialBuilder(_), applications:_*)
  }
  
  def *|(apps: Int)(transf: (MusicalSegment) => MusicalSegment*): ParallelSegment = {
    val applications = Stream.continually(transf).flatten.take(apps)
    multiTransf(parallelBuilder(_), applications:_*)
  }
  
  
  def *+(transf: (MusicalSegment) => MusicalSegment*): SequentialSegment = 
    multiTransf(sequentialBuilder(_), transf:_*)
  def *|(transf: (MusicalSegment) => MusicalSegment*): ParallelSegment = 
    multiTransf(parallelBuilder(_), transf:_*)
  
  private def multiTransf[T <: MusicalSegment](builder: (List[MusicalSegment]) => T, transf: ((MusicalSegment) => MusicalSegment)*): T = {
    val iter = (((x: MusicalSegment) => x) +: transf).iterator
    builder(List.fill(transf.size + 1)(iter.next()(this)))
  }
  
  def >>(durations: BPM*): SequentialSegment = sequentialBuilder(durations.map(O(_)).toList ::: (this :: Nil))
  
  def <<(durations: BPM*): SequentialSegment = sequentialBuilder((this :: durations.map(O(_)).toList))
  
  def +(toneRise: Int): MusicalSegment = +>(_ + toneRise)
  def -(toneRed: Int): MusicalSegment = this + (-toneRed)
  
  def is: MusicalSegment = +>(_ is)
  def es: MusicalSegment = +>(_ es)
  
  // divides duration of all notes by frac
  def /(frac: Double): MusicalSegment = +>(_ / frac)
  def withScale(scale: Scale): MusicalSegment = +>(_ withScale scale)
  
  def +>(expandF: Note => MusicalSegment*): MusicalSegment = expand(IsNote, expandF:_*)
  /*
   * expand following given selector, applying periodically functions in given order
   */
  def expand[T <: MusicalSegment](selector: ClassPredicate[T], expandF: T => MusicalSegment*): MusicalSegment = {
    ++> (new TransformList(selector, expandF.zipWithIndex.foldRight(List.empty[Transform[T]]) {(t, l) => 
      Transform(t._1, expandF.size, t._2, -1) :: l
    }))
  }
  
  def ++>[T <: MusicalSegment](transfs: TransformList[T] = TransformList.identity): MusicalSegment = {
    expandRec(transfs.toStream)._1
  }
  
  
  private def expandRec(expandF: TStream): (MusicalSegment, TStream) = {
    if (expandF.head.isDefinedAt(this)) {
      (expandF.head(this), expandF.tail)
    } else this match {
      case n: Note => (n, expandF) // to prevent infinite recursion
      case _ => {
        val (newMelody, newExpandF) = melody.foldLeft((List.empty[MusicalSegment], expandF)) {
          (t, m) =>val (expM, tStr) = m.expandRec(t._2)
          (expM :: t._1, tStr)
        }
        (buildFromMelody(newMelody.reverse), newExpandF)
      }
    }
  }
  
  def appN(appCount: Int)(function: MusicalSegment => MusicalSegment): MusicalSegment = {
    if (appCount < 1) this
    else function(this).appN(appCount - 1)(function)
  }
  
  def swapTo[T <: MusicalSegment](builder: List[MusicalSegment] => T): T = {
    builder(melody)
  }
  
  // TODO implement
  def toPNF: ParallelSegment = ???
  
  /**
   * Ensures that the returned tree does not contain any
   * directly nested SequentialSegment in SequentialSegment and 
   * ParallelSegment in ParallelSegment.
   * Additionally a melody with a single element will be extracted from
   * it's wrapper (regardless of Parallel or Sequential) as it does
   * not make any difference considering it as Parallel or Sequential.
   */
  def flatAll: MusicalSegment
  
  override def toString: String = '\n' + PrettyPrinter(this)
}

abstract class ParallelSegment
  extends MusicalSegment
  with MusicalSegmentLike[ParallelSegment] {
  
  val length = melody.maxBy(_.length).length
  val parDepth = if(!melody.isEmpty) melody.maxBy(_.parDepth).height + 1 else 0
  val seqDepth = if(!melody.isEmpty) melody.maxBy(_.seqDepth).height else 0
  
  val buildFromMelody: List[MusicalSegment] => ParallelSegment = parallelBuilder(_)
  
  def +(that: MusicalSegment): SequentialSegment = this ++ that
  def |(that: MusicalSegment): ParallelSegment = buildFromMelody(this.melody :+ that)
  
  def flatAll: ParallelSegment = buildFromMelody(melody.flatMap(_.flatAll match {
    // by indyction hypothesis, pm contains only notes and sequentialSegment of lenght > 1
    case ParallelSegment(pm) => pm
    // by induction hypothesis, s can only be a parallelSegment of length > 1 or a note
    case SequentialSegment(s :: Nil) => s.melody
    // other cases are Note and sequential segment of length > 1, no extraction
    case other => other :: Nil
  }))
}

object ParallelSegment {
//  def apply(tracks: MusicalSegment*): ParallelSegment = ParallelSegment(tracks.toList)
  def unapply(parallelSegment: ParallelSegment): Option[List[MusicalSegment]] = {
    Some(parallelSegment.melody)
  }
}

abstract class SequentialSegment
  extends MusicalSegment
  with MusicalSegmentLike[SequentialSegment] {
  
  val length = melody.foldLeft(0.0)(_ + _.length)
  val parDepth = if(!melody.isEmpty) melody.maxBy(_.parDepth).height else 0
  val seqDepth = if(!melody.isEmpty) melody.maxBy(_.seqDepth).height + 1 else 0
  
  val buildFromMelody: List[MusicalSegment] => SequentialSegment = sequentialBuilder(_)
  
  def +(that: MusicalSegment): SequentialSegment = buildFromMelody(this.melody :+ that)
  def |(that: MusicalSegment): ParallelSegment = this || that
  
  def flatAll: SequentialSegment = buildFromMelody(melody.flatMap(_.flatAll match {
    case SequentialSegment(sm) => sm
    case ParallelSegment(p :: Nil) => p.melody
    case other => other :: Nil
  }))
}

object SequentialSegment {
//  def apply(tracks: MusicalSegment*): SequentialSegment = SequentialSegment(tracks.toList)
  def unapply(sequentialSegment: SequentialSegment): Option[List[MusicalSegment]] = {
    Some(sequentialSegment.melody)
  }
}


case class Note(val tone: Tone, val duration: BPM)(
    implicit val scale: Scale = Major(C),
    implicit override val parallelBuilder : List[MusicalSegment] => ParallelSegment = Parallel(_),
    implicit override val sequentialBuilder : List[MusicalSegment] => SequentialSegment = Sequential(_))
  extends MusicalSegment
  with MusicalSegmentLike[Note] {
  
  val melody = this :: Nil
  val length = duration.computed
  val parDepth = 0
  val seqDepth = 0
  override lazy val height = 0
  
  val buildFromMelody: List[MusicalSegment] => Note = ((x) => this)
  
  // allows to include a lhs note in a sequential segment
  def +(that: MusicalSegment) = that match {
    case s @ SequentialSegment(melody) => s.buildFromMelody(this :: melody)
    case _ => this ++ that
  }
  // allows to include a lhs note in a parallel segment
  def |(that: MusicalSegment) = that match {
    case p @ ParallelSegment(melody) => p.buildFromMelody(this :: melody)
    case _ => this || that
  }
  
  def flatAll: Note = this
  
  override def is: Note = Note(tone is, duration)
  override def es: Note = Note(tone es, duration)
  
  override def /(frac: Double): Note = Note(tone, duration / frac)
  
  def withDuration(duration: BPM): Note = Note(tone, duration)
  
  override lazy val notes: List[Note] = melody
  
  override def +(toneRise: Int): Note = Note(tone increaseBy toneRise, duration)
  override def withScale(scale: Scale) = Note(tone, duration)(scale)
  
}

case class Parallel(melody: List[MusicalSegment]) extends ParallelSegment
case class Sequential(melody: List[MusicalSegment]) extends SequentialSegment




