package segmentSystem

import rythmics.BPM
import tonalSystem.Tone
import tonalSystem.O

sealed trait MusicalSegment {
  def melody: List[MusicalSegment]
  
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
  
  def length: Double = melody.foldLeft(0.0)((x, y) => x + y.length)
  
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
  
  def notes: List[Note] = melody.flatMap(_.notes)
  
  def toSNF: SequentialSegment = ???
  
  def flatAll: MusicalSegment
}

case class ParallelSegment(tracks: List[MusicalSegment]) extends MusicalSegment{
  def melody = tracks
  
  def expand(appCount: Int, expandF: => Note => MusicalSegment): ParallelSegment =
    ParallelSegment(melody.map(_.expand(appCount, expandF)))
  
  def flatAll: ParallelSegment = ParallelSegment(melody.flatMap(_ match {
    case ParallelSegment(pm) => pm.flatMap(_.flatAll match {
      case ParallelSegment(pm) => pm
      case other => other :: Nil
    })
    case other => other.flatAll :: Nil
  }))
}

object ParallelSegment {
  def apply(tracks: MusicalSegment*): ParallelSegment = ParallelSegment(tracks.toList)
}

case class SequentialSegment(tracks: List[MusicalSegment]) extends MusicalSegment{
  def melody = tracks
  
  def expand(appCount: Int, expandF: => Note => MusicalSegment): SequentialSegment =
    SequentialSegment(melody.map(_.expand(appCount, expandF)))
    
  def flatAll: SequentialSegment = SequentialSegment(melody.flatMap(_ match {
    case SequentialSegment(sm) => sm.flatMap(_.flatAll match {
      case SequentialSegment(sm) => sm
      case other => other :: Nil
    })
    case other => other.flatAll :: Nil
  }))
}

object SequentialSegment {
  def apply(tracks: MusicalSegment*): SequentialSegment = SequentialSegment(tracks.toList)
}


case class Note(val tone: Tone, val duration: BPM) extends MusicalSegment{
  def melody = this :: Nil
  override def length = duration.computed
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
  
  override def notes: List[Note] = this :: Nil
}
