package chord

import tonalSystem.Tone
import tonalSystem.Tone._

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

  def equalModOctaveTones(t1: Tone, t2: Tone): Boolean =
    t1.stepsTo(t2) % 7 == 0 && t1.alter == t2.alter

}

case class Triad(val fun: Tone) extends Chord {
  val tones: List[Tone => Tone] = List(_ increaseBy 0, _ increaseBy 2, _ increaseBy 4)
  override def equals(o: Any) = {
    o match {
      case Triad(t) => equalModOctaveTones(fun, t)
      case _ => false
    }
  }
  override def hashCode = 0
}

case class Seventh(val fun: Tone) extends Chord {
  val tones: List[Tone => Tone] = List(_ increaseBy 0, _ increaseBy 2, _ increaseBy 4, _ increaseBy 6)
  override def equals(o: Any) = {
    o match {
      case Seventh(t) => equalModOctaveTones(fun, t)
      case _ => false
    }
  }
  override def hashCode = 0
}

case object NapSixth extends Chord {
  val fun = II
  val tones: List[Tone => Tone] = List(_ es, _ increaseBy 2, _ increaseBy 4)
}

//for minor harmonic
case class SecDomMinH(fun: Tone) extends Chord {
  //exists in minor : V of III, IV, V, VI
  val tones: List[Tone => Tone] = fun match {
    case III(_, None) => List(_.decreaseBy(3).is, _ decreaseBy 1, _ increaseBy 1, _ increaseBy 3)
    case IV(_, None) => List(_ decreaseBy 3, _.decreaseBy(1).is, _ increaseBy 1, _.increaseBy(3).es)
    case V(_, None) => List(_ decreaseBy 3, _ decreaseBy 1, _.increaseBy(1).es, _.increaseBy(3).es)
    case VI(_, None) => List(_ decreaseBy 3, _.decreaseBy(1).is, _.increaseBy(1).is, _ increaseBy 3)
    case _ => Seventh(fun decreaseBy 3).tones
  }

  override def equals(o: Any) = {
    o match {
      case SecDomMinH(t) => equalModOctaveTones(fun, t)
      case _ => false
    }
  }
  override def hashCode = 0
}

//for minor natural
case class SecDomMinN(fun: Tone) extends Chord {
  //exists in minor : V of III, IV, V, VI, add VII
  val tones: List[Tone => Tone] = fun match {
    case III(_, None) => List(_ decreaseBy 3, _ decreaseBy 1, _ increaseBy 1, _ increaseBy 3)
    case IV(_, None) | VII(_, None) =>
      List(_ decreaseBy 3, _.decreaseBy(1).is, _ increaseBy 1, _ increaseBy 3)
    case V(_, None) => List(_ decreaseBy 3, _ decreaseBy 1, _ increaseBy 1, _.increaseBy(3).es)
    case VI(_, None) => List(_ decreaseBy 3, _.decreaseBy(1).is, _.increaseBy(1).is, _ increaseBy 3)
    case _ => Seventh(fun decreaseBy 3).tones
  }

  override def equals(o: Any) = {
    o match {
      case SecDomMinN(t) => equalModOctaveTones(fun, t)
      case _ => false
    }
  }
  override def hashCode = 0
}

//for major
case class SecDomMaj(fun: Tone) extends Chord {
  //exists in major : V of II, III, IV, V, VI
  val tones: List[Tone => Tone] = fun match {
    case II(_, None) | V(_, None) | VI(_, None) => List(_ decreaseBy 3, _.decreaseBy(1).is, _ increaseBy 1, _ increaseBy 3)
    case III(_, None) => List(_ decreaseBy 3, _.decreaseBy(1).is, _.increaseBy(1).is, _ increaseBy 3)
    case IV(_, None) => List(_ decreaseBy 3, _ decreaseBy 1, _ increaseBy 1, _.increaseBy(3).es)
    case _ => Seventh(fun decreaseBy 3).tones
  }

  override def equals(o: Any) = {
    o match {
      case SecDomMaj(t) => equalModOctaveTones(fun, t)
      case _ => false
    }
  }
  override def hashCode = 0
}

case object EmptyChord extends Chord {
  val fun = O
  val tones = Nil
}