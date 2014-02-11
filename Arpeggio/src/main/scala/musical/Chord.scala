package main.scala.musical

trait Chord {

  val tones: List[Tone]
  val arpeggio: Arpeggio
}

class BasicChord(val tones: List[Tone], val arpeggio: Arpeggio) extends Chord

class MajorChord(tone: Tone,
      val arpeggio: Arpeggio)
    extends Chord {
  val tones = List(tone, tone + 4, tone + 7)
}

class MinorChord(tone: Tone,
      val arpeggio: Arpeggio)
    extends Chord {
  val tones = List(tone, tone + 3, tone + 7)
}

class SilentChord(duration: Double) extends Chord {
  val tones: List[Tone] = Nil
  val arpeggio: Arpeggio = new Arpeggio(Nil, duration)
}