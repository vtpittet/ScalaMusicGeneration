package play

import midiInterface.MelodyPlayer
import rythmics._
import rythmics.{ E => rE }
import rythmics.{ H => rH }
import rythmics.{ T => rT }
import segmentSystem.MusicalSegment
import segmentSystem.SequentialSegment
import tonalSystem.G
import tonalSystem.Tone._
import tonalSystem.Major
import tonalSystem.Minor
import utils.SS
import utils.Print
import utils.PrettyPrinter
import utils.MelodyWriter
import segmentSystem.Sequential
import segmentSystem.ClassPredicate

object QueVive extends App with MelodyWriter {
  implicit val noteDuration = E

  val sop = {
    (I(W) + I(Q) + V(-1) + I(Q) + II + III + IV + III(Q) + II + I(Q-) +
      IV(Q) + IV + IV(Q) + IV + V + IV + III(H) + III(H) +
      II(Q) + II + II + II + II + III + II + III(H) + III(H) +
      IV(Q) + IV + IV(Q) + V + IV(Q) + III(W) +
      II(Q) + II + II(Q) + III + II(Q) + I(W)) + 7
  }

  val alt = {
    (I(1)(W) + I(1)(Q) + V + I(1)(Q) + V + V + V + I(1)(Q) + VII + VI(Q-) +
      IV(Q) + IV + IV(Q) + IV + IV + IV + V(H) + V(H) +
      VII.is(Q) + VII.is + VII.is + VII.is + VII.is + VII.is + VII.is + I(1)(H) + I(1)(H) +
      VII(Q) + VII + VII(Q) + VII + VII(Q) + VII(W) +
      VII.is(Q) + VII.is + V(Q) + V + V(Q) + V(W))
  }

  val ten = {
    (I(W) + I(Q) + V(-1) + I(Q) + IV + III + II + III(Q) + IV + III(Q-) +
      II(Q) + II + II(Q) + II + II + II + III(H) + III(H) +
      IV(Q) + IV + IV + IV + IV + V + IV + III(H) + V(H) +
      IV(Q) + IV + IV(Q) + IV + IV(Q) + V(W) +
      IV(Q) + IV + IV(Q) + IV + IV(Q) + III(W))
  }

  val bas = {
    (I(W) + I(Q) + V(-1) + I(Q) + I + I + I + ((VI(Q) + VI + VI(Q-) +
      VII(Q) + VII + VII(Q) + VII + VII + VII + III(H) + III(H) +
      V(Q) + V + V + V + V + V + V) - 7) + I(H) + I(H) +
      II(Q) + II + II(Q) + II + II(Q) + III(W) +
      ((V(Q) + V + VII.is(Q) + VII.is + VII.is(Q)) - 7) + I(W))
  }

  def compose(s1: MusicalSegment, s2: MusicalSegment, b1: MusicalSegment, b2: MusicalSegment): MusicalSegment =
    (s1 | s2 | b1 | b2)

  val part = compose(sop, alt, ten, bas) - 7

  val tempo = 120
  val instrument = 0
  val minScale = Minor(G)

  MelodyPlayer(
    Sequential(Nil)
      + (part withScale minScale),
    tempo,
    //    fromQN = 3*15,
    //    toQN = 3*15,
    instrument = instrument)

}
