package tonalSystem

// TODO discuss option of creating pitches through a stream-like form to avoid multiple instantiation
sealed trait Pitch { self =>
  val octave: Int
  val alter: Int
  
  val next: Pitch
  val prev: Pitch
  
  val newSelf: (Int, Int) => Pitch
  
  def +(steps: Int): Pitch = {
    if (steps < 0) prev - (steps + 1)
    else if (steps == 0) self
    else next + (steps - 1)
  }
  def -(steps: Int): Pitch = self + (-steps)
  
  def up(halfTones: Int): Pitch = {
    if (halfTones <= -7) quintDown up (halfTones + 7)
    else if (halfTones < -1) self - 1 up (halfTones + 2)
    else if (halfTones == -1) self es
    else if (halfTones == 0) self
    else if (halfTones == 1) self is
    else if (halfTones < 7) self + 1 up (halfTones - 2)
    else /*if (tones >= 7)*/ quintUp up (halfTones - 7)
  }
  def down(halfTones: Int): Pitch = self + (-halfTones)
  
  private lazy val quintUp: Pitch = self match {
    case B(o, a) => F(o, a+1)
    case _ => self + 4
  }
  private lazy val quintDown: Pitch = self match {
    case F(o, a) => B(o, a-1)
    case _ => self - 4
  }
  
  def is(times: Int): Pitch = newSelf(octave, alter+times)
  def is: Pitch = is(1)
  def es(times: Int): Pitch = is(-times)
  def es: Pitch = is(-1)
  
  def isSameStep(that: Pitch): Boolean = (this, that) match {
    case (A(_, _), A(_, _)) => true
    case (B(_, _), B(_, _)) => true
    case (C(_, _), C(_, _)) => true
    case (D(_, _), D(_, _)) => true
    case (E(_, _), E(_, _)) => true
    case (F(_, _), F(_, _)) => true
    case (G(_, _), G(_, _)) => true
    case _ => false
  }
}

case object S extends Pitch {
  val octave = 0
  val alter = 0
  val prev = S
  val next = S
  val newSelf = (o: Int, a: Int) => S
}

trait Tonality

case class A(octave: Int, alter: Int) extends Pitch with Tonality {
  lazy val next = B(octave, alter)
  lazy val prev = G(octave - 1, alter)
  
  val newSelf = (o: Int, a: Int) => A(o, a)
}

case class B(octave: Int, alter: Int) extends Pitch with Tonality {
  lazy val next = C(octave, alter)
  lazy val prev = A(octave, alter)
  
  val newSelf = (o: Int, a: Int) => B(o, a)
}

case class C(octave: Int, alter: Int) extends Pitch with Tonality {
  lazy val next = D(octave, alter)
  lazy val prev = B(octave, alter)
  
  val newSelf = (o: Int, a: Int) => C(o, a)
}

case class D(octave: Int, alter: Int) extends Pitch with Tonality {
  lazy val next = E(octave, alter)
  lazy val prev = C(octave, alter)
  
  val newSelf = (o: Int, a: Int) => D(o, a)
}

case class E(octave: Int, alter: Int) extends Pitch with Tonality {
  lazy val next = F(octave, alter)
  lazy val prev = D(octave, alter)
  
  val newSelf = (o: Int, a: Int) => E(o, a)
}

case class F(octave: Int, alter: Int) extends Pitch with Tonality {
  lazy val next = G(octave, alter)
  lazy val prev = E(octave, alter)
  
  val newSelf = (o: Int, a: Int) => F(o, a)
}

case class G(octave: Int, alter: Int) extends Pitch with Tonality {
  lazy val next = A(octave + 1, alter)
  lazy val prev = F(octave, alter)
  
  val newSelf = (o: Int, a: Int) => G(o, a)
}



object A extends A(0, 0)
object B extends B(0, 0)
object C extends C(0, 0)
object D extends D(0, 0)
object E extends E(0, 0)
object F extends F(0, 0)
object G extends G(0, 0)
