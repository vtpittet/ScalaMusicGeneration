package gen

import segmentSystem._
import tonalSystem.Tone
import tonalSystem.Tone._
import scala.util.Random
import chord.Chord
import segmentSystem.ClassPredicate._

case class HarmonyGen(melody: SequentialSegment, allChords: List[Chord]) {

  def harmonize: MusicalSegment = {

    val flatMel = melody.flatAll
    var melv: MusicalSegment = flatMel
    if (flatMel.parDepth != 0) {
      melv = getOneVoice(flatMel)
    }
    val mel = melv

    //TODO for now, 2 octaves below the lowest note of the melody 
    //TODO if melody too low : put it higher
    val lowerBound = mel.notes.min(NoteOrdering) - 14

    val possibleChords: List[List[Chord]] = mel.notes.map(getPossChords(_.tone))

    //TODO for now : take randomly one chord for each note
    val chosenChords = possibleChords.map(x => chooseOneIn(x))

    ???
  }
  def getPossChords(t: Tone): List[Chord] = {
    allChords.filter(_.contains())
  }

  def getOneVoice(mel: MusicalSegment): MusicalSegment = {
    if (mel.parDepth != 0) {
      getOneVoice(mel mapIf (isPar thenDo (_.melody.head)))
    } else mel
  }

  def chooseOneIn[A](l: List[A]): A = {
    Random.shuffle(l).head
  }

}

//TODO : test and then put in Tone after asking Valerian
object NoteOrdering extends Ordering[Note] {
  def compare(a: Note, b: Note) = {
    val at = a.tone
    val bt = b.tone
    if (at.stepsTo(bt) < 0) 1
    else if (at.stepsTo(bt) > 0) -1
    else if (at.alter == bt.alter) 0
    else if (at.alter.isDefined) {
      if (at.alter.get) 1 else -1
    } else {
      if (bt.alter.get) -1 else 1
    }
  }
}



