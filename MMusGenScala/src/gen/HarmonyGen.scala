package gen

import segmentSystem._
import tonalSystem.Tone
import tonalSystem.Tone._
import scala.util.Random
import chord.Chord
import segmentSystem.ClassPredicate._
import scala.collection.mutable.ListBuffer

case class HarmonyGen(melody: MusicalSegment, allChords: List[Chord]) {

  def harmonize: (MusicalSegment, ParallelSegment) = {

    val flatMel = melody.flatAll
    var melv: MusicalSegment = flatMel
    if (flatMel.parDepth != 0) {
      melv = getOneVoice(flatMel)
    }
    val mel = melv

    //TODO for now, 2 octaves below the lowest note of the melody 
    //TODO if melody too low : put it higher
    val lowerBound = mel.notes.min(NoteOrdering) - 14
    val melT = mel.notes map (_.tone)
    val possibleChords: List[List[Chord]] = melT map (getPossChords(_))
    val chosenChords = findChords(possibleChords)
    val chosenTonesL = findAllTones(chosenChords, melT)
    val chosenNotes = tonesToNotes(chosenTonesL, mel.notes)

    (mel, createPar(transpose(chosenNotes).tail))

  }
  def getPossChords(t: Tone): List[Chord] = {
    allChords.filter(_.contains(t))
  }

  // from http://stackoverflow.com/questions
  // /1683312/is-there-a-safe-way-in-scala-to-transpose-a-list-of-unequal-length-lists
  def transpose[A](xss: List[List[A]]): List[List[A]] = {
    val buf = new ListBuffer[List[A]]
    var yss = xss
    while (!yss.head.isEmpty) {
      buf += (yss map (_.head))
      yss = (yss map (_.tail))
    }
    buf.toList
  }
  def tonesToNotes(tones: List[List[Tone]], notes: List[Note]): List[List[Note]] = {
    (tones zip notes) map (x => x._1 map (y => Note(y, x._2.duration)))
  }

  def createPar(voices: List[List[Note]]): ParallelSegment = {
    def toSequ(voice: List[Note]): SequentialSegment = {
      voice.foldLeft[SequentialSegment](EmptySeq)((s, n) => s + n)
    }
    voices.tail.foldLeft(EmptyPar | toSequ(voices.head))((x, y) => x | toSequ(y))
  }

  def findChords(poss: List[List[Chord]]): List[Chord] = {
    //TODO for now : take randomly one chord for each note
    poss.map(x => chooseOneIn(x))
  }

  def findAllTones(chords: List[Chord], mel: List[Tone] /*, lowerBound: Tone*/ ): List[List[Tone]] = {

    def findTones(pred: List[Tone], curr: Chord, currm: Tone): List[Tone] = {
      ???
    }
    ???
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



