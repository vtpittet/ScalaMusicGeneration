package caseStudy

import tonalSystem.Major
import segmentSystem.MusicalSegment
import tonalSystem.Minor
import utils.MelodyWriter
import segmentSystem.SequentialSegment
import utils.SS
import midiInterface.MelodyPlayer
import rythmics.{T=>rT}
import rythmics.{E=>rE}
import rythmics.{H=>rH}
import segmentSystem.ClassPredicate.isNote
import tonalSystem.A
import segmentSystem.ClassPredicate.isSeq
import segmentSystem.Sequential
import tonalSystem.Tone._
import segmentSystem.ClassPredicate
import segmentSystem.Note

object RecuerdosP1 extends App with MelodyWriter {
  
  case class StdSop(melody: List[MusicalSegment]) extends SequentialSegment {
    override val buildFromMelody = StdSop(_)
  }
  object isStd extends ClassPredicate[StdSop](_ match {case n: StdSop => n})
  
  case class SpecSop(melody: List[MusicalSegment]) extends SequentialSegment {
    override val buildFromMelody = SpecSop(_)
  }
  object isSpec extends ClassPredicate[SpecSop](_ match {case n: SpecSop => n})
  
  // nothing to do, duration set through implicit val
  def bass2(b: MusicalSegment): MusicalSegment = b // mapNotes { _ withDuration (rH-) }
  
  // sets duration to rE and shift each note of rE
  def bass1(b: MusicalSegment): MusicalSegment = b mapNotes { O(rE) + _ }
  
  def sopran2(s: MusicalSegment): MusicalSegment =
    (s mapNotes { _ withDuration rE }).mapNotes(O(rE) + _, identity) mapNotes { _ + O(rE) }
  
    
  def simpleSopran1(s: MusicalSegment): MusicalSegment = 
    s mapNotes { O(rT) + _.withDuration(rT) *3 }
  
  def stdSopran1(s: MusicalSegment): MusicalSegment =
    simpleSopran1(s mapNotes (_.withDuration(rT)*4, _.withDuration(rT)*2))
  
  def specSopran1(s: MusicalSegment): MusicalSegment = s mapNotes (
    _.withDuration(rT) *2 mapNotes (
      O(rT) + _ *3,
      O(rT) + _.fillSeq(_ / (1.5), _.+(1) / (1.5), _ / (1.5))),
    _.withDuration(rT) *4 mapNotes { O(rT) + _ *3 })
  
  def sopran1(s: MusicalSegment): MusicalSegment = {
    s mapIf (isStd thenDo (stdSopran1(_))) mapIf (isSpec thenDo (specSopran1(_)))
//    s mapIf (isNote thenDo
//      (n => {
//        (n *3 >> rT) + (n + (n/1.5 *+ (_ + 1, identity)) >> rT)
//      }, 8, 20) or
//      (_.*(3).>>(rT) * 4, 8, 21) or
//      (_.*(3).>>(rT) * 4, 2) or
//      (_.*(3).>>(rT) * 2, 2, 1)
//    
//    )
  }
  
  
  def compose(s1: MS, s2: MS, b1: MS, b2: MS): MS = {
    sopran1(s1) | sopran2(s2) | bass1(b1) | bass2(b2)
  }
  
  
  def s11Pattern1(base: N, finalStep: Int): SS = {
    base fillSeq (_-1, _-2, _-1, _*3, _+finalStep) swapTo {StdSop(_)}
  }
  def s11Pattern2(base: N, altStart: Int = 0, altEnd: Int = 0): MS = {
    def alterWith(a: Int): Note => Note = a match {
      case x if x == 1 => _.is
      case x if x == -1 => _.es
      case _ => identity
    }
    // heavy definitions ! How to do it lighter ?
    
//    (base *+ (_-1, _-2, _-1) swapTo {StdSop(_)}) ++
//    (base-2 + (base-3) swapTo {SpecSop(_)}) +
//    ((base-3) *2 swapTo {StdSop(_)})
    
    val m = (base *6) mapNotes (
        alterWith(altStart)(_),
        _-1, _-2, _-1, _-2,
        { x => (alterWith(altEnd)(x) - 3) *3})
    
    m.flatAll.groupBy(2) mapIf (
        isSeq given(_.height == 1)
        thenDo (_ swapTo (SpecSop(_)), 1, 2, 3)
        orDo (_ swapTo (StdSop(_))))
  }
  
  val s11 = {
    implicit val noteDuration = rT
    V + VII + III(1) + II(1) + V mapNotes (
        s11Pattern1(_, 1),
        s11Pattern1(_, 0),
        s11Pattern2(_, altEnd = 1),
        s11Pattern2(_, altStart = -1),
        s11Pattern2(_)
    )
    /*
    (V + IV + III + IV + V + V + V + VI +
    VII + VI + V + VI + VII + VII + VII + VII +
    III(1) + II(1) + I(1) + II(1)) ++ (I(1) + VII/*.is*/) + (VII/*.is*/ + VII/*.is*/ +
    II(1)/*.es*/ + I(1) + VII + I(1)) + (VII + VI) + (VI + VI +
    V + IV + III + IV) + (III + II) + (II + II)
    */
  }
    
  val s12 = {
    implicit val noteDuration = rE
    s11Pattern1(III, 1) +
    s11Pattern1(V, 0) +
    (I(1) fillSeq (_-1, _-2, _-11, _ -3, _-3, _-3, _-3)) +
    (V fillSeq (_-0, _-0, _+1, _-1, _-1, _-1, _-1)) +
    (II fillSeq (_-0, _-1, _-1, _.is-2, _.is-2, _.is-2, _.is-2))
    /*
    III + II + I + II + III + III + III + IV +
    V + IV + III + IV + V + V + V + V +

    I(1) + VII + VI + IV(-1) + V *4 +
    V *2 + V + VI + IV *4 +
    II *2 + I *2 + VII(-1).is *4
    * 
    */
  }
  
  def b11Pattern1(base: Note, finalStep: Int): SS = {
    base * 11 + (base + finalStep)
  }
  
  val b11 = {
    implicit val noteDuration = rE
    b11Pattern1(V(-1), 2) +
    b11Pattern1(VII(-1), 0) +
    /*
    V(-1) *3 *3 + V(-1) + V(-1) + VII(-1) +
    VII(-1) *3 *(3 + 1) +
    */
    III *3 + III *2 + VI + II *3 + IV *3 +
    III.is *3 + I *3 + IV + I *(2 + 3 +
    3) + V(-1).es *3 + V(-1) *3 + V(-1) *3
  }
    
  val b12 = {
    implicit val noteDuration = (rH-)
    I(-1) *(3 + 1) +
    III(-1) *(3 + 1) +
    VI(-1) *2 + V(-2) + V(-2) +
    I(-1) + III(-1).is + IV(-1) + IV(-1) +
    IV(-1) + VI(-2) + V(-2) + V(-2)
  }
  
  val part1 = compose(s11, s12, b11, b12)
  
  val tempo = 120
  val instrument = 0
  val minScale = Minor(A)
  val majScale = Major(A)

  // Short version without repetitions
  MelodyPlayer(
    Sequential(Nil)
    + ((part1) withScale minScale)
    ,
    tempo,
//    fromQN = 3*3,
//    toQN = 3*4,
    instrument = instrument
  )
}