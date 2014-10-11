package gen

import segmentSystem._
import tonalSystem.Tone
import tonalSystem.Tone._
import scala.util.Random

case class HarmonyGen(sMelody: SimpleMelody /*, lowerBound: Note*/ , allChords: List[CharChord]) {

  def harmonize: MusicalSegment = {
    //test if lowerBound consistent (not higher than the melody) at least

    //TODO for now, 2 octaves below the lowest note of the melody 
    val lowerBound = sMelody.melody.min(NoteOrdering) - 14

    val possibleChords: List[List[CharChord]] = sMelody.melody.map(x => getPossChords(Ut.toneToDegree(x.tone)))

    //TODO for now : take randomly one chord for each note
    val chosenChords = possibleChords.map(x => chooseOneIn(x))

    ???
  }
  def getPossChords(d: Degree): List[CharChord] = {
    allChords.filter(x => x.belongsToChord(d))
  }

  def chooseOneIn[A](l: List[A]): A = {
    Random.shuffle(l).head //TODO does this change the original ? (I think not but have a doubt)
  }

}

//TODO : perhaps bad structure : is type of Tone without octaves 
//TODO how to force char of degree being between 0 to 7 ?
//TODO perhaps simply use one tone to characterize it ?
// char is the characteristic tone of the degree : from 1 to 7 normally
//can be zero when unknown
case class Degree(char: Int, alt: Option[Boolean]) {
  def +(degRise: Int): Degree = {
    return new Degree(Ut.posMod(char - 1 + degRise, 7) + 1, alt);
  }
}

object Ut {
  def toneToDegree(t: Tone): Degree = {
    t match {
      case O => Degree(0, None)
      case I(_, alt) => Degree(1, alt)
      case II(_, alt) => Degree(2, alt)
      case III(_, alt) => Degree(3, alt)
      case IV(_, alt) => Degree(4, alt)
      case V(_, alt) => Degree(5, alt)
      case VI(_, alt) => Degree(6, alt)
      case VII(_, alt) => Degree(7, alt)
    }
  }
  def degreeToTone(d: Degree, octave: Int): Tone = {
    val i = posMod(d.char - 1, 7) + 1
    if (i == 0) O
    else if (i == 1) I(octave, d.alt)
    else if (i == 2) II(octave, d.alt)
    else if (i == 3) III(octave, d.alt)
    else if (i == 4) IV(octave, d.alt)
    else if (i == 5) V(octave, d.alt)
    else if (i == 6) VI(octave, d.alt)
    else VII(octave, d.alt)
  }

  def posMod(i: Int, mod: Int): Int = {
    return (i % mod + mod) % mod
  }

  def toneIsLTOrE(t1: Tone, t2: Tone): Boolean = {
    if (t1.octave != t2.octave) (t1.octave <= t2.octave)
    else (toneToDegree(t1).char <= toneToDegree(t2).char)
  }
}

object NoteOrdering extends Ordering[Note] {
  def compare(a: Note, b: Note) = {
    if (a.tone.octave != b.tone.octave) { if (a.tone.octave < b.tone.octave) -1 else 1 }
    else if (Ut.toneToDegree(a.tone).char < Ut.toneToDegree(b.tone).char) -1
    else if (Ut.toneToDegree(a.tone).char > Ut.toneToDegree(b.tone).char) 1
    else 0
  }
}

//chord with list of notes characterized by a degree, and perhaps with other things that will be added
// (for triads and simple seventh it is ok like that)
trait CharChord {
  val degrees: List[Degree] // first is the bass of the chord
  def belongsToChord(d: Degree): Boolean = {
    degrees.contains(d) //TODO : force Degree be 0 to 7 only, or here use modulo
  }
}

case class Triad(degree: Degree) extends CharChord {
  val degrees = degree :: (degree + 2) :: (degree + 4) :: Nil
}

case class SeventhCh(degree: Degree) extends CharChord {
  val degrees = degree :: (degree + 2) :: (degree + 4) :: (degree + 6) :: Nil
}

case class Chord4(notes: List[Tone], template: CharChord) {

}


