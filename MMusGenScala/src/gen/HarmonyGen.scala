package gen

import segmentSystem._
import tonalSystem.Tone
import tonalSystem.Tone._
import scala.util.Random
import chord._
import segmentSystem.ClassPredicate._
import scala.collection.mutable.ListBuffer

case class HarmonyGen(melody: MusicalSegment) {

  val allChords: List[Chord] = List(Triad(I), Triad(II), Triad(III),
    Triad(IV), Triad(V), Triad(VI), Triad(VII), Seventh(V))
  //TODO add all what is in the grammar
  //TODO : should be ChInv perhaps ?

  def harmonize(): (MusicalSegment, ParallelSegment) = {
    val flatMel = melody.flatAll
    var melv: MusicalSegment = flatMel
    if (flatMel.parDepth != 0) {
      melv = getOneVoice(flatMel)
    }
    val mel = melv

    //TODO for now, 2 octaves below the lowest note of the melody 
    //TODO if melody too low : put it higher (of octaves)
    //TODO or : put always same bound : I(0)
    // and put higher the melody if there is a note lower than some threshold ?
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
    //	after : use prevPoss
    //poss.map(x => chooseOneIn(x))

    ???
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

  //TODO : way to give a list of possible chords from a grammar (for allPoss of HarmonyGen ?)
  //-- then : define a trait of harmonyGenerizers !!
  //TODO : perhaps a problem : changes the octave to 0
  def prevPoss(ci: ChI) /* extends PartialFunction[??]*/ : List[ChInv] = {
    ci match {
      case ChInv(Triad(I(_, None)), i) if testInv(i) => HarmFct(V)
      case ChInv(Triad(II(_, None)), i) if testInv(i) => HarmFct(I)
      //case ChInv(Triad(III(_, None)), i) if testInv(i) => ???
      case ChInv(Triad(IV(_, None)), i) if testInv(i) => HarmFct(I)
      case ChInv(Triad(V(_, None)), i) if testInv(i) => HarmFct(I) ::: HarmFct(IV)
      case ChInv(Triad(VI(_, None)), i) if testInv(i) => getCiL(V, i)
      case ChInv(Triad(VII(_, None)), i) if testInv(i) => HarmFct(I) ::: HarmFct(IV)
      case ChInv(Triad(I(_, None)), i) if testInv(i, List(Inv3)) => ???
      //case ChInv(Seventh(V(_, None)), i) if testInv(i) => ??? //TODO : find legal inversions for V7+
      //TODO : add others
      case End => getCiL(I, List(Inv1)) ::: getCiL(IV, List(Inv1)) ::: getCiL(V, List(Inv1))
      case EndHalf => getCiL(V, List(Inv1))
      case _ => Nil
    }
  }

  //TODO perhaps better way to do patter matching with list
  def testInv(i: List[Inversion], li: List[Inversion] = List(Inv1, Inv2)): Boolean = {
    i.filter(x => li.contains(x)).nonEmpty
  }

  def HarmFct(t: Tone): List[ChInv] = {
    t match {
      case I(_, None) => List(I, VI).map(x => ChInv(Triad(x), List(Inv1, Inv2)))
      case V(_, None) => ChInv(Triad(I), List(Inv3)) :: //I64
        ChInv(Seventh(V), List(Inv1, Inv2, Inv3, Inv4)) :: //V7+ //TODO find leagal inversion
        List(V, VII).map(x => ChInv(Triad(x), List(Inv1, Inv2)))
      case IV(_, None) => List(IV, II).map(x => ChInv(Triad(x), List(Inv1, Inv2)))
      case _ => Nil
    }
  }

  def getCiL(t: Tone, i: List[Inversion]): List[ChInv] = List(t).map(x => ChInv(Triad(x), i))

  /*
  def followPoss(c: Chord) /* extends PartialFunction[??]*/ : List[List[Chord]] = {
    ???
  }*/

}

//TODO : test and then put in Note / Tone after asking Valerian
object NoteOrdering extends Ordering[Note] {
  def compare(a: Note, b: Note) = {
    ToneOrdering.compare(a.tone, b.tone)
  }
}
object ToneOrdering extends Ordering[Tone] {
  def compare(a: Tone, b: Tone) = {
    if (a.stepsTo(b) < 0) 1
    else if (a.stepsTo(b) > 0) -1
    else if (a.alter == b.alter) 0
    else if (a.alter.isDefined) {
      if (a.alter.get) 1 else -1
    } else {
      if (b.alter.get) -1 else 1
    }
  }
}

class ChI
case class ChInv(c: Chord, i: List[Inversion]) extends ChI
case object End extends ChI
case object EndHalf extends ChI

//TODO : perhaps represent differently ?
trait Inversion
case object Inv1 extends Inversion
case object Inv2 extends Inversion
case object Inv3 extends Inversion
case object Inv4 extends Inversion
