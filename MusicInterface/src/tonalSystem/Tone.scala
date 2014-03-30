package tonalSystem

import segmentSystem.Note
import rythmics.BPM._
import rythmics.BPM
import rythmics.Q


sealed trait Tone {
  
  
  
  
  def is = alterate(true)
  def es = alterate(false)
  
  private def alterate(isSharp: Boolean): Tone = alter match {
    case Some(isSharp) => this
    case _ => newTone(octave, Some(isSharp))
  }
  
  private def newTone(octave: Int, alt: Option[Boolean]): Tone = this match {
    case O => O
    case I(_, _) => I(octave, alt)
    case II(_, _) => II(octave, alt)
    case III(_, _) => III(octave, alt)
    case IV(_, _) => IV(octave, alt)
    case V(_, _) => V(octave, alt)
    case VI(_, _) => VI(octave, alt)
    case VII(_, _) => VII(octave, alt)
  }
  
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
  
  def stepsTo(that: Tone): Int = {
    if (this == O || that == O) 0
    else (that.octave - this.octave) * 7 + that.typeToInt - this.typeToInt
  }
  
  private def typeToInt: Int = this match {
    case O => 0
    case I(_, _) => 0
    case II(_, _) => 1
    case III(_, _) => 2
    case IV(_, _) => 3
    case V(_, _) => 4
    case VI(_, _) => 5
    case VII(_, _) => 6
  }
  /*
   * Some(true) => +1/2
   * None => + 0
   * Some(false) => -1/2
   */
  val alter: Option[Boolean]
  
  val octave: Int
}

case object O extends Tone {
  val octave = 0
  val alter = None
  def apply(duration: BPM = Q): Note =
    Note(O, duration)
}

case class I(val octave: Int, val alter: Option[Boolean]) extends Tone
case class II(val octave: Int, val alter: Option[Boolean]) extends Tone
case class III(val octave: Int, val alter: Option[Boolean]) extends Tone
case class IV(val octave: Int, val alter: Option[Boolean]) extends Tone
case class V(val octave: Int, val alter: Option[Boolean]) extends Tone
case class VI(val octave: Int, val alter: Option[Boolean]) extends Tone
case class VII(val octave: Int, val alter: Option[Boolean]) extends Tone


trait ApplyToNote {
  
  def apply(duration: BPM = Q, octave: Int = 0, alter: Option[Boolean] = None): Note
  def apply(octave: Int): Note = apply(Q, octave)
}

object I extends I(0, None) with ApplyToNote {
  def apply(duration: BPM = Q, octave: Int = 0, alter: Option[Boolean] = None): Note =
    Note(I(octave, alter), duration)
}
object II extends II(0, None) with ApplyToNote {
  def apply(duration: BPM = Q, octave: Int = 0, alter: Option[Boolean] = None): Note =
    Note(II(octave, alter), duration)
}
object III extends III(0, None) with ApplyToNote {
  def apply(duration: BPM = Q, octave: Int = 0, alter: Option[Boolean] = None): Note =
    Note(III(octave, alter), duration)
}
object IV extends IV(0, None) with ApplyToNote {
  def apply(duration: BPM = Q, octave: Int = 0, alter: Option[Boolean] = None): Note =
    Note(IV(octave, alter), duration)
}
object V extends V(0, None) with ApplyToNote {
  def apply(duration: BPM = Q, octave: Int = 0, alter: Option[Boolean] = None): Note =
    Note(V(octave, alter), duration)
}
object VI extends VI(0, None) with ApplyToNote {
  def apply(duration: BPM = Q, octave: Int = 0, alter: Option[Boolean] = None): Note =
    Note(VI(octave, alter), duration)
}
object VII extends VII(0, None) with ApplyToNote {
  def apply(duration: BPM = Q, octave: Int = 0, alter: Option[Boolean] = None): Note =
    Note(VII(octave, alter), duration)
}

