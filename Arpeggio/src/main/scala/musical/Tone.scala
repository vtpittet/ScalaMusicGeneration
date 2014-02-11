package main.scala.musical


sealed trait Tone {
  self =>
  val octave: Int
  def +(halfTone: Int): Tone
  def -(halfTone: Int): Tone
}


// FIXME bug with A+(-1)
case class A(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new AS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new A(octave-1) + (12-halfTone)
}

case class AS(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new B(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new AS(octave-1) + (12-halfTone)
}

case class B(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new C(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new B(octave-1) + (12-halfTone)
}

case class C(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new CS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new C(octave-1) + (12-halfTone)
}
case class CS(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new D(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new CS(octave-1) + (12-halfTone)
}
case class D(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new DS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new D(octave-1) + (12-halfTone)
}
case class DS(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new E(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new DS(octave-1) + (12-halfTone)
}
case class E(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new F(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new E(octave-1) + (12-halfTone)
}
case class F(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new FS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new F(octave-1) + (12-halfTone)
}
case class FS(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new G(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new FS(octave-1) + (12-halfTone)
}
case class G(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new GS(octave)+(halfTone-1)
  }
  def -(halfTone: Int): Tone = new G(octave-1) + (12-halfTone)
}
case class GS(octave: Int) extends Tone {
  def +(halfTone: Int): Tone = if (halfTone == 0) {
    this
  } else {
    new A(octave + 1) + (halfTone-1)
  }
  def -(halfTone: Int): Tone = new GS(octave-1) + (12-halfTone)
}

// Rest
case object O extends Tone {
  val octave = 0
  def +(halfTone: Int) = this
  def -(halfTone: Int) = this
}