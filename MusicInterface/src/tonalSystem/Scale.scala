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
    case I(_) => 0
    case II(_) => 2
    case III(_) => 4
    case IV(_) => 5
    case V(_) => 7
    case VI(_) => 9
    case VII(_) => 11
  }
}

case class MinorNatural(val tonic: Pitch) extends Scale {
  val mode = tonic + " Natural Minor"
  def degreeToHalves(tone: Tone): Int = tone match {
    case O => 0
    case I(_) => 0
    case II(_) => 2
    case III(_) => 3
    case IV(_) => 5
    case V(_) => 7
    case VI(_) => 8
    case VII(_) => 10
  }
}

case class MinorHarmonic(val tonic: Pitch) extends Scale {
  val mode = tonic + " Harmonic Minor"
  def degreeToHalves(tone: Tone): Int = tone match {
    case O => 0
    case I(_) => 0
    case II(_) => 2
    case III(_) => 3
    case IV(_) => 5
    case V(_) => 7
    case VI(_) => 8
    case VII(_) => 11
  }
}