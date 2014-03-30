package segmentSystem

import rythmics.BPM
import tonalSystem.Tone
import tonalSystem.O

sealed trait MusicalSegment {
  def melody: List[MusicalSegment]
  
  // parallel addition
  def |(that: MusicalSegment): ParallelSegment = ParallelSegment(List(this, that))
  // sequential addition
  def +(that: MusicalSegment): SequentialSegment = SequentialSegment(List(this, that))
  
  
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
  
  
  def length: Double = melody.foldLeft(0.0)((x, y) => x + y.length)
  
  def +(toneRise: Int): MusicalSegment = +>((v: Note) => Note(v.tone + toneRise, v.duration))
  def -(toneRed: Int): MusicalSegment = this + (-toneRed)
  
  // divides duration of all notes by frac
  def /(frac: Double): MusicalSegment = +>((v: Note) => Note(v.tone, v.duration / frac))
  
  def +> : (Note => MusicalSegment*) => MusicalSegment = +>(1)
  
  def +>(appCount: Int)(expandF: Note => MusicalSegment*): MusicalSegment = {
    val iter: () => Note => MusicalSegment = Stream.continually(expandF).flatten.iterator.next
    expand(appCount, iter)
  }
  
  def expand(appCount: Int, expandF: () => Note => MusicalSegment): MusicalSegment
  
}

case class ParallelSegment(tracks: List[MusicalSegment]) extends MusicalSegment{
  def melody = tracks
  
  def expand(appCount: Int, expandF: () => Note => MusicalSegment): ParallelSegment =
    ParallelSegment(melody.map(_.expand(appCount, expandF)))
}
case class SequentialSegment(tracks: List[MusicalSegment]) extends MusicalSegment{
  def melody = tracks
  
  def expand(appCount: Int, expandF: () => Note => MusicalSegment): SequentialSegment =
    SequentialSegment(melody.map(_.expand(appCount, expandF)))
}
case class Note(val tone: Tone, val duration: BPM) extends MusicalSegment{
  def melody = this :: Nil
  override def length = duration.computed
  def expand(appCount: Int, expandF: () => Note => MusicalSegment) = if(appCount < 1) {
    this
  } else if (appCount == 1) {
    expandF()(this)
  } else {
    (expandF()(this)).expand(appCount-1, expandF)
  }
  
  def is: Note = Note(tone is, duration)
  def es: Note = Note(tone es, duration)
}
