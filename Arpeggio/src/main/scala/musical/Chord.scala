package main.scala.musical

trait Chord {

  val tones: List[Tone]
  val arpeggio: Arpeggio
}

case class BasicChord(val tones: List[Tone], val arpeggio: Arpeggio) extends Chord

case class MajorChord(tone: Tone,
      val arpeggio: Arpeggio)
    extends Chord {
  val tones = List(tone, tone + 4, tone + 7)
}

case class MinorChord(tone: Tone,
      val arpeggio: Arpeggio)
    extends Chord {
  val tones = List(tone, tone + 3, tone + 7)
}

case class SilentChord(duration: Double) extends Chord {
  val tones: List[Tone] = Nil
  val arpeggio: Arpeggio = new Arpeggio(Nil, duration)
}