package tonalSystem

sealed trait Scale {
  type Mode = String
  
  val mode: Mode
  val tonic: Pitch
  // translate Tone to corresponding pitch according to the scale
  def pitchTone(tone: Tone): Pitch = tonic + degreeToHalves(tone) + tone.octave * 12
  
  // returns how many half tones separate the given tone with the tonic (assume in same octave)
  def degreeToHalves(tone: Tone): Int
  
}


case class Major(val tonic: Pitch) extends Scale {
  val mode = tonic + " Major"
  def degreeToHalves(tone: Tone): Int = tone match {
    case O => 0
    case I(_, _) => 0
    case II(_, _) => 2
    case III(_, _) => 4
    case IV(_, _) => 5
    case V(_, _) => 7
    case VI(_, _) => 9
    case VII(_, _) => 11
  }
}

case class MinorNatural(val tonic: Pitch) extends Scale {
  val mode = tonic + " Natural Minor"
  def degreeToHalves(tone: Tone): Int = tone match {
    case O => 0
    case I(_, _) => 0
    case II(_, _) => 2
    case III(_, _) => 3
    case IV(_, _) => 5
    case V(_, _) => 7
    case VI(_, _) => 8
    case VII(_, _) => 10
  }
}

case class MinorHarmonic(val tonic: Pitch) extends Scale {
  val mode = tonic + " Harmonic Minor"
  def degreeToHalves(tone: Tone): Int = tone match {
    case O => 0
    case I(_, _) => 0
    case II(_, _) => 2
    case III(_, _) => 3
    case IV(_, _) => 5
    case V(_, _) => 7
    case VI(_, _) => 8
    case VII(_, _) => 11
  }
}