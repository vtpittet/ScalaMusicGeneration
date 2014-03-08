package tonalSystem

sealed trait Tone {
  
  lazy val next: Tone = this match {
    case I(o, a) => II(o, a)
    case II(o, a) => III(o, a)
    case III(o, a) => IV(o, a)
    case IV(o, a) => V(o, a)
    case V(o, a) => VI(o, a)
    case VI(o, a) => VII(o, a)
    case VII(o, a) => I(o + 1, a)
    case O => O
  }
  lazy val prev: Tone = this match {
    case I(o, a) => VII(o - 1, a)
    case II(o, a) => I(o, a)
    case III(o, a) => II(o, a)
    case IV(o, a) => III(o, a)
    case V(o, a) => IV(o, a)
    case VI(o, a) => V(o, a)
    case VII(o, a) => VI(o, a)
    case O => O
  }
  
  def +(tones: Int): Tone = if (tones < 0) {
    prev + (tones + 1)
  } else if (tones == 0) {
    this
  } else {
    next + (tones - 1)
  }
  
  def -(tones: Int): Tone = this + (-tones)
  
  def <(that: Tone): Boolean = ???
  
  val alter: Int
  
  val octave: Int
}

case object O extends Tone {
  val octave = 0
  val alter = 0
}

case class I(val octave: Int = 0, val alter: Int = 0) extends Tone
case class II(val octave: Int = 0, val alter: Int = 0) extends Tone
case class III(val octave: Int = 0, val alter: Int = 0) extends Tone
case class IV(val octave: Int = 0, val alter: Int = 0) extends Tone
case class V(val octave: Int = 0, val alter: Int = 0) extends Tone
case class VI(val octave: Int = 0, val alter: Int = 0) extends Tone
case class VII(val octave: Int = 0, val alter: Int = 0) extends Tone

object I extends I(0, 0)
object II extends II(0, 0)
object III extends III(0, 0)
object IV extends IV(0, 0)
object V extends V(0, 0)
object VI extends VI(0, 0)
object VII extends VII(0, 0)

