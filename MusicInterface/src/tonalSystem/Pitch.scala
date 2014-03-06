package tonalSystem

sealed trait Pitch {
  val octave: Int
  
  def +(halfTone: Int): Pitch
  def -(halfTone: Int): Pitch
}


case class A(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new AS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new A(octave-1) + (12-halfTone)
}

case class AS(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new B(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new AS(octave-1) + (12-halfTone)
}

case class B(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new C(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new B(octave-1) + (12-halfTone)
}

case class C(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new CS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new C(octave-1) + (12-halfTone)
}
case class CS(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new D(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new CS(octave-1) + (12-halfTone)
}
case class D(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new DS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new D(octave-1) + (12-halfTone)
}
case class DS(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new E(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new DS(octave-1) + (12-halfTone)
}
case class E(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new F(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new E(octave-1) + (12-halfTone)
}
case class F(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new FS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new F(octave-1) + (12-halfTone)
}
case class FS(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new G(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new FS(octave-1) + (12-halfTone)
}
case class G(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new GS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Pitch = new G(octave-1) + (12-halfTone)
}
case class GS(octave: Int) extends Pitch {
  def +(halfTone: Int): Pitch = if (halfTone == 0) {
    this
  } else {
    new A(octave + 1) + (halfTone-1)
  }
  def -(halfTone: Int): Pitch = new GS(octave-1) + (12-halfTone)
}

// Rest
case object S extends Pitch {
  val octave = 0
  def +(halfTone: Int) = S
  def -(halfTone: Int) = S
}


object A extends A(0)
object AS extends AS(0)
object B extends B(0)
object C extends C(0)
object CS extends CS(0)
object D extends D(0)
object DS extends DS(0)
object E extends E(0)
object F extends F(0)
object FS extends FS(0)
object G extends G(0)
object GS extends GS(0)
