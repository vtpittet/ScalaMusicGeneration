import tonalSystem.Tone
import tonalSystem.Tone.II

sealed trait Chord {
  val fun: Tone
  val tones: List[Tone => Tone]

  def apply(n: Int): Tone = {
    def posMod(i: Int, mod: Int): Int = (i % mod + mod) % mod
    tones(posMod(n, tones.size))(fun) increaseBy (n / tones.size * 7)
  }

  def contains(t: Tone): Boolean = {
    (tones map (_(fun)) filter (x => x.stepsTo(t) % 7 == 0 && x.alter == t.alter)) nonEmpty
  }
}

case class Triad(val fun: Tone) extends Chord {
  val tones: List[Tone => Tone] = List(_ increaseBy 0, _ increaseBy 2, _ increaseBy 4)
}

case class Seventh(val fun: Tone) extends Chord {
  val tones: List[Tone => Tone] = List(_ increaseBy 0, _ increaseBy 2, _ increaseBy 4, _ increaseBy 6)
}

case object NapSixth extends Chord {
  val fun = II
  val tones: List[Tone => Tone] = List(_ es, _ increaseBy 2, _ increaseBy 4)
}
