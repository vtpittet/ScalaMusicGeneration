package segmentSystem

import rythmics.BPM
import tonalSystem.O
import tonalSystem.Tone
import utils.PrettyPrinter
import tonalSystem.Scale
import tonalSystem.Major
import tonalSystem.C

sealed trait MusicalSegment {
  
  type TStream = Stream[PartialFunction[MusicalSegment, MusicalSegment]]
  
  
  val melody: List[MusicalSegment]
  
  lazy val height: Int = if(!melody.isEmpty) melody.maxBy(_.height).height + 1 else 0
  
  val parDepth: Int
  val seqDepth: Int
  val length: Double
  
  lazy val notes: List[Note] = melody.flatMap(_.notes)
  
  val buildFromMelody: List[MusicalSegment] => MusicalSegment
  
  // addition
  /*
   * Note: the parameter depth represents the depth of resulting MusicalSegment
   * 
   * Implementation choice : in the case where depth > max depth of params this and that are put as
   * melody of a new MusicalSegment, then this result is put itself as a melody in a new MusicalSegment
   * as long as the resulting depth is smaller than the requested depth in the function call.
   */
  private[segmentSystem] def add[T <: MusicalSegment](depth: Int, builder: List[MusicalSegment] => T, that: MusicalSegment): T = {
    if (this.height > depth || that.height > depth) this.add(scala.math.max(this.height, that.height), builder, that)
    else if (this.height < depth && that.height < depth) {
      def encloseToDepth(aimDepth: Int, segment: T): T = if (segment.height < aimDepth) {
        encloseToDepth(aimDepth, builder(List(segment)))
      } else segment
      encloseToDepth(depth, builder(List(this, that)))
    }
    else if (this.height ==depth && that.height ==depth) builder(this.melody ::: that.melody)
    else if (this.height < depth && that.height ==depth) builder(this :: that.melody)
    else/*if(this.depth ==depth && that.depth < depth)*/ builder(this.melody :+ that)
  }
  
  // parallel addition
  def |(that: MusicalSegment): ParallelSegment = ParallelSegment(this, that)
  /* use cases for readablility and consistency. should be able to call those ops on any arg*/
  def |*(depth: Int, that: MusicalSegment): ParallelSegment = this | that
  def ||(that: MusicalSegment): ParallelSegment = this | that
  def |||(that: MusicalSegment): ParallelSegment = this | that
  def ||||(that: MusicalSegment): ParallelSegment = this | that
  
  // sequential addition
  def +(that: MusicalSegment): SequentialSegment = SequentialSegment(this, that)
  def +*(depth: Int, that: MusicalSegment): SequentialSegment = this + that
  def ++(that: MusicalSegment): SequentialSegment = this + that
  def +++(that: MusicalSegment): SequentialSegment = this + that
  def ++++(that: MusicalSegment): SequentialSegment = this + that
  
  /*
   * Note that parallel multiplication is nearly idempotent as the same melody is replayed at the same time
   * def *(repetitions: Int): ParallelSegment = ParallelSegment(List.fill(repetitions)(this))
   */
  def *(repetitions: Int): SequentialSegment = SequentialSegment(List.fill(repetitions)(this))
  
  // assume identity at the beginning of each definition
  def *+(apps: Int)(transf: (MusicalSegment) => MusicalSegment*): SequentialSegment = {
    val applications = Stream.continually(transf).flatten.take(apps)
    multiTransf(SequentialSegment(_), applications:_*)
  }
  
  def *|(apps: Int)(transf: (MusicalSegment) => MusicalSegment*): ParallelSegment = {
    val applications = Stream.continually(transf).flatten.take(apps)
    multiTransf(ParallelSegment(_), applications:_*)
  }
  
  
  def *+(transf: (MusicalSegment) => MusicalSegment*): SequentialSegment = 
    multiTransf(SequentialSegment(_), transf:_*)
  def *|(transf: (MusicalSegment) => MusicalSegment*): ParallelSegment = 
    multiTransf(ParallelSegment(_), transf:_*)
  
  private def multiTransf[T <: MusicalSegment](builder: (List[MusicalSegment]) => T, transf: ((MusicalSegment) => MusicalSegment)*): T = {
    val iter = (((x: MusicalSegment) => x) +: transf).iterator
    builder(List.fill(transf.size + 1)(iter.next()(this)))
  }
  
  def >>(durations: BPM*): SequentialSegment = SequentialSegment(durations.map(O(_)).toList ::: (this :: Nil))
  
  def <<(durations: BPM*): SequentialSegment = SequentialSegment((this :: durations.map(O(_)).toList))
  
  def +(toneRise: Int): MusicalSegment = +>((v: Note) => Note(v.tone increaseBy toneRise, v.duration))
  def -(toneRed: Int): MusicalSegment = this + (-toneRed)
  
  // divides duration of all notes by frac
  def /(frac: Double): MusicalSegment = +>((v: Note) => Note(v.tone, v.duration / frac))
  
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
  
  // TODO implement
  def toPNF: ParallelSegment = ???
  
  /**
   * This function ensure that the returned tree does not contain any
   * directly nested SequentialSegment in SequentialSegment and 
   * ParallelSegment in ParallelSegment.
   * Additionally a melody with a single element will be extracted from
   * it's wrapper (regardless of Parallel or Sequential) as it does
   * not make any difference considering it as Parallel or Sequential.
   */
  def flatAll: MusicalSegment
  
  override def toString: String = '\n' + PrettyPrinter(this)
}

/*
 * Depth-controlling operators moved to those traits to allow Note implementig them.
 * Need extending Musical Segment to overload correctly default |, + operators
 * defined in MusicalSegment
 */
sealed trait Parallel extends MusicalSegment {
  
  // depth-controlled operation on parallel composition of parallel segments
  def |*(depth: Int, that: MusicalSegment with Parallel): ParallelSegment = add(depth, ParallelSegment(_), that)
  def |(that: MusicalSegment with Parallel): ParallelSegment = this |* (1, that)
  def ||(that: MusicalSegment with Parallel): ParallelSegment = this |* (2, that)
  def |||(that: MusicalSegment with Parallel): ParallelSegment = this |* (3, that)
  def ||||(that: MusicalSegment with Parallel): ParallelSegment = this |* (4, that)
}

sealed trait Sequential extends MusicalSegment {
  
  // depth-controlled operation on sequential composition of sequential segments
  def +*(depth: Int, that: MusicalSegment with Sequential): SequentialSegment = add(depth, SequentialSegment(_), that)
  def +(that: MusicalSegment with Sequential): SequentialSegment = this +* (1, that)
  def ++(that: MusicalSegment with Sequential): SequentialSegment = this +* (2, that)
  def +++(that: MusicalSegment with Sequential): SequentialSegment = this +* (3, that)
  def ++++(that: MusicalSegment with Sequential): SequentialSegment = this +* (4, that)
}


case class ParallelSegment(melody: List[MusicalSegment]) extends Parallel {
  
  val length = melody.maxBy(_.length).length
  val parDepth = if(!melody.isEmpty) melody.maxBy(_.parDepth).height + 1 else 0
  val seqDepth = if(!melody.isEmpty) melody.maxBy(_.seqDepth).height else 0
  
  val buildFromMelody: List[MusicalSegment] => ParallelSegment = ParallelSegment(_)
  
  def flatAll: ParallelSegment = ParallelSegment(melody.flatMap(_.flatAll match {
    // by indyction hypothesis, pm contains only notes and sequentialSegment of lenght > 1
    case ParallelSegment(pm) => pm
    // by induction hypothesis, s can only be a parallelSegment of length > 1 or a note
    case SequentialSegment(s :: Nil) => s.melody
    // other cases are Note and sequential segment of length > 1, no extraction
    case other => other :: Nil
  }))
}

object ParallelSegment {
  def apply(tracks: MusicalSegment*): ParallelSegment = ParallelSegment(tracks.toList)
}

case class SequentialSegment(melody: List[MusicalSegment]) extends Sequential {
  
  val length = melody.foldLeft(0.0)(_ + _.length)
  val parDepth = if(!melody.isEmpty) melody.maxBy(_.parDepth).height else 0
  val seqDepth = if(!melody.isEmpty) melody.maxBy(_.seqDepth).height + 1 else 0
  
  val buildFromMelody: List[MusicalSegment] => SequentialSegment = SequentialSegment(_)
  
  def flatAll: SequentialSegment = SequentialSegment(melody.flatMap(_.flatAll match {
    case SequentialSegment(sm) => sm
    case ParallelSegment(p :: Nil) => p.melody
    case other => other :: Nil
  }))
}

object SequentialSegment {
  def apply(tracks: MusicalSegment*): SequentialSegment = SequentialSegment(tracks.toList)
}


case class Note(val tone: Tone, val duration: BPM) extends MusicalSegment with Parallel with Sequential {
  val melody = this :: Nil
  val length = duration.computed
  val parDepth = 0
  val seqDepth = 0
  override lazy val height = 0
  
  val buildFromMelody: List[MusicalSegment] => Note = x => this
  
  def flatAll: Note = this
  
  def is: Note = Note(tone is, duration)
  def es: Note = Note(tone es, duration)
  
  def withDuration(duration: BPM): Note = Note(tone, duration)
  
  override lazy val notes: List[Note] = melody
  
}
