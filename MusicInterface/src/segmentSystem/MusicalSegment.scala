package segmentSystem

import rythmics.BPM
import tonalSystem.Tone
import tonalSystem.O
import utils.PrettyPrinter

sealed trait MusicalSegment {
  val melody: List[MusicalSegment]
  
  val depth: Int = melody.maxBy(_.depth).depth + 1
  val length: Double
  
  // parallel addition
  def |(that: MusicalSegment): ParallelSegment = ParallelSegment(this, that)
  // sequential addition
  def +(that: MusicalSegment): SequentialSegment = SequentialSegment(this, that)
  
  
  /*
   * Note that parallel multiplication is nearly idempotent as the same melody is replayed at the same time
   * def *(repetitions: Int): ParallelSegment = ParallelSegment(List.fill(repetitions)(this))
   */
  def *(repetitions: Int): SequentialSegment = SequentialSegment(List.fill(repetitions)(this))
  
//  TODO is it good to assume identity at each beginning of definition ?
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
  
  def >>(duration: BPM*): SequentialSegment = SequentialSegment(duration.foldRight(this :: Nil)((d, s) => O(d) :: s))
  
  def <<(duration: BPM*): SequentialSegment = SequentialSegment((this :: duration.map(O(_)).toList))
  
  def +(toneRise: Int): MusicalSegment = +>((v: Note) => Note(v.tone + toneRise, v.duration))
  def -(toneRed: Int): MusicalSegment = this + (-toneRed)
  
  // divides duration of all notes by frac
  def /(frac: Double): MusicalSegment = +>((v: Note) => Note(v.tone, v.duration / frac))
  
  def +>(expandF: Note => MusicalSegment*): MusicalSegment = +>(1)(expandF:_*)
  
  def +>(appCount: Int)(expandF: Note => MusicalSegment*): MusicalSegment = {
    val iter = Stream.continually(expandF).flatten.iterator
    expand(appCount, iter.next())
  }
  
  def expand(appCount: Int, expandF: => Note => MusicalSegment): MusicalSegment
  
  /*
   * TODO : this is implementable, how without many code duplication ?
   * - builder of instances (how to ?)
   * 
   * 
  def +>(expandF: MusicalSegment => MusicalSegment*): MusicalSegment = +>(1, expandF:_*)
  def +>(appCount: Int, expandF: MusicalSegment => MusicalSegment*): MusicalSegment = {
    val iter = Stream.continually(expandF).flatten.iterator
    expand(appCount, iter.next)
  }
  
  def expand(appCount: Int, expandF: => MusicalSegment => MusicalSegment): MusicalSegment
  * 
  */
  
  lazy val notes: List[Note] = melody.flatMap(_.notes)
  
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

case class ParallelSegment(melody: List[MusicalSegment]) extends MusicalSegment{
  
  val length = melody.maxBy(_.length).length
  
  def expand(appCount: Int, expandF: => Note => MusicalSegment): ParallelSegment =
    ParallelSegment(melody.map(_.expand(appCount, expandF)))
  
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

case class SequentialSegment(melody: List[MusicalSegment]) extends MusicalSegment{
  
  val length = melody.foldLeft(0.0)(_ + _.length)
  
  def expand(appCount: Int, expandF: => Note => MusicalSegment): SequentialSegment =
    SequentialSegment(melody.map(_.expand(appCount, expandF)))
  
  def flatAll: SequentialSegment = SequentialSegment(melody.flatMap(_.flatAll match {
    case SequentialSegment(sm) => sm
    case ParallelSegment(p :: Nil) => p.melody
    case other => other :: Nil
  }))
}

object SequentialSegment {
  def apply(tracks: MusicalSegment*): SequentialSegment = SequentialSegment(tracks.toList)
}


case class Note(val tone: Tone, val duration: BPM) extends MusicalSegment{
  val melody = this :: Nil
  override val depth = 0
  override val length = duration.computed
  def expand(appCount: Int, expandF: => Note => MusicalSegment) = if(appCount < 1) {
    this
  } else if (appCount == 1) {
    expandF(this)
  } else {
    (expandF(this)).expand(appCount-1, expandF)
  }
  
  def flatAll: Note = this
  
  def is: Note = Note(tone is, duration)
  def es: Note = Note(tone es, duration)
  
  def withDuration(duration: BPM): Note = Note(tone, duration)
  
  override lazy val notes: List[Note] = melody
}
