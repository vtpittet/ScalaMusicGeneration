package rythmics

case class BPM(val computed: Double) {
  def /(frac: Double) = if (frac == 0) BPM(0) else BPM(computed/frac)
  def - = BPM(computed * 1.5)
}

object BPM {
  def apply(full: Int, frac: Int = 0): BPM = BPM(full + {if(frac > 0) 1.0/frac else 0})
  val w = BPM(4.0)
  val h = BPM(2.0)
  val q = BPM(1.0)
  val e = BPM(0.5)
  val s = BPM(0.25)
  val t = BPM(0.125)
}

// whole note
object W extends BPM(4.0)
// halve note
object H extends BPM(2.0)
// quarter note
object Q extends BPM(1.0)
// eight note
object E extends BPM(0.5)
// sixteenth note
object S extends BPM(0.25)
// thirty-second note
object T extends BPM(0.125)
