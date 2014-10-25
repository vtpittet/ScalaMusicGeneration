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

  def harmonize(endF: ChiEnd): (MusicalSegment, ParallelSegment) = {
    val flatMel = melody.flatAll
    var melv: MusicalSegment = flatMel
    if (flatMel.parDepth != 0) {
      melv = getOneVoice(flatMel)
    }
    val mel = melv

    //TODO for now, 2 octaves below the lowest note of the melody 
    //TODO if melody too low : put it higher (of octaves) -> then : tell back to caller !!
    //TODO or : put always same bound : I(0)
    // and put higher the melody if there is a note lower than some threshold ? -> then : tell back to caller !!
    val lowerBound = mel.notes.min(NoteOrdering) - 14
    val melT = mel.notes map (_.tone)
    val possibleChords: List[List[Chord]] = melT map (getPossChords(_))
    val chosenChords = findChords(possibleChords, endF)
    val chosenTonesL = findAllTones(chosenChords, melT)
    val chosenNotes = tonesToNotes(chosenTonesL, mel.notes)

    (mel, createPar(transpose(chosenNotes).tail))

  }
  def getPossChords(t: Tone): List[Chord] = {
    //TODO : change : give here ChInv with poss Invs !!
    // see possInv
    t match {
      case O => List(EmptyChord)
      case _ => allChords.filter(_.contains(t))
    }
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

  def findChords(poss: List[List[Chord]], endF: ChI): List[ChInv] = {

    def findChord(i: ChI, possC: List[List[Chord]], buf: List[ChInv]): List[ChInv] = {
      if (possC.isEmpty) return buf
      else if (possC.head.isEmpty) {
        //bizarre note
        //TODO if bizarre note : put some triad of it,
        // or put a harmonically correct chord ? (-> dissonance) ?
        if (buf.isEmpty) {
          // at the end
          val nextC = ChInv(EmptyChord, Nil)
          findChord(nextC, possC.tail, nextC :: buf)
        } else {
          //keep the previous chord
          val nextC = buf.head
          findChord(nextC, possC.tail, nextC :: buf)
        }
      } else if (possC.head.head == EmptyChord) {
        //silent : give silent chord, but comtinues harmony for next
        findChord(i, possC.tail, ChInv(EmptyChord, Nil) :: buf)
      }

      val possChI = possC.head.map(x => ChInv(x, possInv(x)))
      val inter: List[ChInv] = intersectChInv(possChI, prevPoss(i))
      if (inter.isEmpty) {
        //no possible chord is harmonically ok,
        // but there are possible chords (possC.head isn't empty)
        if (buf.nonEmpty && intersectChInv(possChI, List(buf.head)).nonEmpty) {
          //give the previous if it is possible
          val nextC = intersectChInv(possChI, List(buf.head)).head
          findChord(nextC, possC.tail, nextC :: buf)
        } else {
          //take a possible chord, even if harmonically not ok
          val nextC = Random.shuffle(possChI).head
          findChord(nextC, possC.tail, nextC :: buf)
        }

      } else {
        //normal case : random between ok chords
        val nextC = Random.shuffle(inter).head
        findChord(nextC, possC.tail, nextC :: buf)
      }

    }
    def possInv(c: Chord): List[Inversion] = {
      c match {
        case EmptyChord => Nil
        case Triad(I(_, _)) => List(Inv1, Inv2, Inv3)

        case Triad(_) => List(Inv1, Inv2)
        case Seventh(_) => List(Inv1, Inv2, Inv3, Inv4)

        //TODO : add other special cases

        case _ if (c.tones.size == 3) => List(Inv1, Inv2, Inv3)
        case _ if (c.tones.size == 4) => List(Inv1, Inv2, Inv3, Inv4)
        case _ => Nil // TODO : perhaps not the best to do
      }
    }
    def intersectChInv(a: List[ChInv], b: List[ChInv]): List[ChInv] = {
      val ca = a.map(_.c)
      val cb = b.map(_.c)

      ???

    }

    findChord(endF, poss.reverse, Nil)
  }

  def findAllTones(chords: List[ChInv], mel: List[Tone] /*, lowerBound: Tone*/ ): List[List[Tone]] = {
    //TODO : fct that put away some inversions from a list of chord
    // (ex : if V7+ -> I, I has to be Inv1, can't be I6)

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
  //TODO : perhaps a problem : fct changes the octave to 0
  def prevPoss(ci: ChI) /* extends PartialFunction[??]*/ : List[ChInv] = {
    ci match {
      case ChInv(Triad(I(_, None)), i) if testInv(i) => HarmFct(V)
      case ChInv(Triad(II(_, None)), i) if testInv(i) => HarmFct(I)
      //case ChInv(Triad(III(_, None)), i) if testInv(i) => ???
      case ChInv(Triad(IV(_, None)), i) if testInv(i) => HarmFct(I)
      case ChInv(Triad(V(_, None)), i) if testInv(i) => HarmFct(I) ::: HarmFct(IV)
      case ChInv(Triad(VI(_, None)), i) if testInv(i) => getCiL(V, i)
      case ChInv(Triad(VII(_, None)), i) if testInv(i) => HarmFct(I) ::: HarmFct(IV)
      //case ChInv(Triad(I(_, None)), i) if testInv(i, List(Inv3)) => ???
      case ChInv(Seventh(V(o, None)), i) if testInv(i) => prevPoss(ChInv(Triad(V(o, None)), List(Inv1, Inv2)))
      //TODO : add others
      case End => getCiL(I, List(Inv1)) ::: getCiL(IV, List(Inv1)) ::: getCiL(V, List(Inv1))
      case EndHalf => getCiL(V, List(Inv1))
      case _ => Nil
    }
  }

  //TODO perhaps better way to do pattern matching with list
  def testInv(i: List[Inversion], li: List[Inversion] = List(Inv1, Inv2)): Boolean = {
    i.filter(x => li.contains(x)).nonEmpty
  }

  def HarmFct(t: Tone): List[ChInv] = {
    t match {
      case I(_, None) => List(I, VI).map(x => ChInv(Triad(x), List(Inv1, Inv2)))
      case V(_, None) => ChInv(Triad(I), List(Inv3)) :: //I64
        //ChInv(Seventh(V), List(Inv1, Inv2, Inv3, Inv4)) :: //V7+ //TODO : before I only -> pb
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
class ChiEnd extends ChI
case object End extends ChiEnd
case object EndHalf extends ChiEnd

//TODO : perhaps represent differently ?
trait Inversion
case object Inv1 extends Inversion
case object Inv2 extends Inversion
case object Inv3 extends Inversion
case object Inv4 extends Inversion
