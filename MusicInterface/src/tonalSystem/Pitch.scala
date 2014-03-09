package tonalSystem

sealed trait Pitch { self =>
  val octave: Int
  val alter: Int
  
  val next: Pitch
  val prev: Pitch
  
  val newSelf: (Int, Int) => Pitch
  
  def +(steps: Int): Pitch = {
    if (steps < 0) prev - (steps + 1)
    else if (steps == 0) self
    else prev + (steps - 1)
  }
  def -(steps: Int): Pitch = self + (-steps)
  
  def is(times: Int = 1) = newSelf(octave, alter+times)
  def es(times: Int = 1) = is(-times)
}

case object S extends Pitch {
  val octave = 0
  val alter = 0
  val prev = S
  val next = S
  val newSelf = (o: Int, a: Int) => S
}

case class A(octave: Int, alter: Int) extends Pitch {
  lazy val next = B(octave, alter)
  lazy val prev = G(octave - 1, alter)
  
  val newSelf = (o: Int, a: Int) => A(o, a)
}

case class B(octave: Int, alter: Int) extends Pitch {
  lazy val next = C(octave, alter)
  lazy val prev = A(octave, alter)
  
  val newSelf = (o: Int, a: Int) => B(o, a)
}

case class C(octave: Int, alter: Int) extends Pitch {
  lazy val next = D(octave, alter)
  lazy val prev = B(octave, alter)
  
  val newSelf = (o: Int, a: Int) => C(o, a)
}

case class D(octave: Int, alter: Int) extends Pitch {
  lazy val next = E(octave, alter)
  lazy val prev = C(octave, alter)
  
  val newSelf = (o: Int, a: Int) => D(o, a)
}

case class E(octave: Int, alter: Int) extends Pitch {
  lazy val next = F(octave, alter)
  lazy val prev = D(octave, alter)
  
  val newSelf = (o: Int, a: Int) => E(o, a)
}

case class F(octave: Int, alter: Int) extends Pitch {
  lazy val next = G(octave, alter)
  lazy val prev = E(octave, alter)
  
  val newSelf = (o: Int, a: Int) => F(o, a)
}

case class G(octave: Int, alter: Int) extends Pitch {
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
