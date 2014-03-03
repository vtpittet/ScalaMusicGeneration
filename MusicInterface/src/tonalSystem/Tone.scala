package tonalSystem

sealed trait Tone {
  
  lazy val next: Tone = this match {
    case I(o) => II(o)
    case II(o) => III(o)
    case III(o) => IV(o)
    case IV(o) => V(o)
    case V(o) => VI(o)
    case VI(o) => VII(o)
    case VII(o) => I(o + 1)
    case O => O
  }
  lazy val prev: Tone = this match {
    case I(o) => VII(o - 1)
    case II(o) => I(o)
    case III(o) => II(o)
    case IV(o) => III(o)
    case V(o) => IV(o)
    case VI(o) => V(o)
    case VII(o) => VI(o)
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
  
  val octave: Int
}

case object O extends Tone {
  val octave = 0
}

case class I(val octave: Int = 0) extends Tone
case class II(val octave: Int = 0) extends Tone
case class III(val octave: Int = 0) extends Tone
case class IV(val octave: Int = 0) extends Tone
case class V(val octave: Int = 0) extends Tone
case class VI(val octave: Int = 0) extends Tone
case class VII(val octave: Int = 0) extends Tone

object I extends I(0)
object II extends II(0)
object III extends III(0)
object IV extends IV(0)
object V extends V(0)
object VI extends VI(0)
object VII extends VII(0)

