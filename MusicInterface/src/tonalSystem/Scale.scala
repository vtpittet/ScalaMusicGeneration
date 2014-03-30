package tonalSystem

import scala.collection.immutable.Stream

// TODO Solve key signature cannot be in Scale trait !!!
sealed trait Scale {
  val tonic: Pitch with Tonality
  
  // this value is used to construct raw pitches and then apply alteration
  private val normalizedTonic: Pitch with Tonality = tonic match {
    case A(o, _) => A(o, 0)
    case B(o, _) => B(o, 0)
    case C(o, _) => C(o, 0)
    case D(o, _) => D(o, 0)
    case E(o, _) => E(o, 0)
    case F(o, _) => F(o, 0)
    case G(o, _) => G(o, 0)
  }
  // translate Tone to corresponding pitch according to the scale
  def pitchTone(tone: Tone): Pitch = tone match {
    case O => S
    case tone => {
      val pitch = alter(normalizedTonic + (I.stepsTo(tone)))
      tone.alter match {
        case None => pitch
        case Some(true) => pitch is
        case Some(false) => pitch es
      }
    }
  }
  
  // apply scale's alteration to the given pitch
  def alter(pitch: Pitch): Pitch = {
    val filtered = keySignature.filter(_.isSameStep(pitch))
    pitch is (filtered.size * scala.math.signum(alterationCount))
  }
  
  
  val alterationCount: Int
  
  lazy val keySignature: List[Pitch with Tonality] = {
    val circleOfFifths = if (alterationCount < 0) {
      Stream.continually(List(B, E, A, D, G, C, F)).flatten.iterator
    } else {
      Stream.continually(List(F, C, G, D, A, E, B)).flatten.iterator
    }
    List.fill(scala.math.abs(alterationCount))(circleOfFifths.next)
  }
}


case class Major(val tonic: Pitch with Tonality) extends Scale {
  
  override val alterationCount: Int = (tonic.alter * 7) + (tonic match {
    case F(_, _) => -1
    case C(_, _) => 0
    case G(_, _) => 1
    case D(_, _) => 2
    case A(_, _) => 3
    case E(_, _) => 4
    case B(_, _) => 5
  })
}

case class Minor(val tonic: Pitch with Tonality) extends Scale {
  
  override val alterationCount: Int = (tonic.alter * 7) + (tonic match {
    case F(_, _) => -4
    case C(_, _) => -3
    case G(_, _) => -2
    case D(_, _) => -1
    case A(_, _) => 0
    case E(_, _) => 1
    case B(_, _) => 2
  })
  
}