package caseStudy

import tonalSystem.Major
import segmentSystem.MusicalSegment
import tonalSystem.Minor
import tonalSystem.Scale
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
import tonalSystem._
import segmentSystem.ClassPredicate
import segmentSystem.Note

object NewRecuerdosP1 extends App with MelodyWriter {
  
  def newMakeMesureT1(scale: Scale, ts: Int, tb: Int): MS = {
    makeMesureT1(V, III, V(-1), I(-1), ts, tb) withScale scale
  }
  
  def newMakeMesureT1ret(scale: Scale, ts: Int, tb: Int): MS = {
    makeMesureT1(III, I, V(-1), I(-1), ts, tb) withScale scale
  }
  
  def makeMesureT1(s1: Tone, s2: Tone, b1: Tone, b2: Tone, ts: Int, tb: Int): MS = {
    
    val fs1 = {
      implicit val noteDuration = rT
      s1 *4 + (s1 + ts) *2 mapNotes { O + _ *3 }
    }
    val fs2b1 = {
      implicit val noteDuration = rE
      O + b1 + s2 + b1 + (s2 + ts) + (b1 + tb)
    }
    
    val fb2 = b2(rH-)
    
    fs1 | fs2b1 | fb2
  }
  
  def phraseT1(tonic: Tone, ret: Boolean, ts: Int, tb: Int): MS = {
    val b2 = tonic
    val b1 = tonic increaseBy 4
    val s12 = tonic increaseBy (7 + 2)
    val s11 = tonic increaseBy (7 + 4)
    val ts1 = if (ret) -1 else 0
    val tb1 = 0
    
    val s22 = if (ret) tonic increaseBy 7 else s12
    val s21 = if (ret) tonic increaseBy (7 + 2) else s11
    
    val m1 = makeMesureT1(s11, s12, b1, b2, ts1, tb1)
    
    val m2 = makeMesureT1(s21, s22, b1, b2, ts, tb)
    
    m1 + m2
  }
  
  def newPhraseT1(scale: Scale, ret: Boolean, ts: Int, tb: Int): MS = {
    val m1 = newMakeMesureT1(scale, ts, tb)
    val m2 = if (ret) {
      newMakeMesureT1ret(scale, ts, tb)
    } else {
      newMakeMesureT1(scale, ts, tb)
    }
    m1 + m2
  }
  
  def makeMesureT2(s1: Tone, s2: Tone, b1: Tone, b2: Tone): MS = {
    val fs1 = {
      implicit val noteDuration = rT
      (s1 + 1) *2 + (s1 *4) mapIf (isNote
        thenDo ({n => O + (n fillSeq (_ /(2/3), _ /(2/3) + 1, _/(2/3)))}, 1, 1, 2)
        orDo (O + _ *3))
    }
    val fs2b1 = {
      implicit val noteDuration = rE
      O + b1 + s2 + b1 + s2 + b1
    }
    val fb2 = b2(rH-)
    fs1 | fs2b1 | fb2
  }
  
  
  
  val aaCCF = {
    phraseT1(I(-1), true, 1, 0) +
    phraseT1(I(-1), false, 1, 1) +
    phraseT1(III(-1), true, 1, 0) +
    phraseT1(III(-1), false, 0, 0) +
    phraseT1(VI(-1), true, 1, 1)
  }
  
  val aaCCF2 = {
    newPhraseT1(Minor(A), true, 1, 0) +
    newPhraseT1(Minor(A), false, 1, 1) +
    newPhraseT1(Major(C), true, 1, 0) +
    newPhraseT1(Major(C), false, 0, 0) +
    newPhraseT1(Major(F), true, 1, 1)
  }
  
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
    (II fillSeq (_-0, _-1, _-1, _.is - 2, _.is - 2, _.is - 2, _.is - 2))
    /*
    III + II + I + II + III + III + III + IV +
    V + IV + III + IV + V + V + V + V +

    I(1) + VII + VI + IV(-1) + V *4 +
    V *2 + V + VI + IV *4 +
    II *2 + I *2 + VII(-1).is *4
    * 
    */
  }
  
  def b11Pattern(base: Note, initStep: Int = 0, finalStep: Int = 0): SS = {
    (base + initStep) + base + (base + finalStep)
  }
  
  val b11rootroot = {
    implicit val noteDuration = rE
    V(-1) + VII(-1) + III + I + V(-1)
  }
  
  val b11root = {
    implicit val noteDuration = rE
    (V(-1) *4) +
    (VII(-1) *4) +
    (III fillSeq (_+0, _+1, _-1)) +
    ((I fillSeq 4) (_.is+2 , _+0, _+0, _+0)) +
    ((V(-1) fillSeq 4) (_+3, _.es+0, _+0, _+0))
  }
  
  val b11 = {
    implicit val noteDuration = rE
    b11Pattern(V(-1)) +
    b11Pattern(V(-1)) +
    b11Pattern(V(-1)) +
    b11Pattern(V(-1), finalStep = 2) +
    //
    b11Pattern(VII(-1)) +
    b11Pattern(VII(-1)) +
    b11Pattern(VII(-1)) +
    b11Pattern(VII(-1)) +
    //
    b11Pattern(III) +
    b11Pattern(III, finalStep = 3) +
    b11Pattern(II) + 
    b11Pattern(IV) +
    //
    b11Pattern(III.is) +
    b11Pattern(I) +
    b11Pattern(I, initStep = 4) +
    b11Pattern(I) +
    //
    b11Pattern(I) +
//    b11Pattern(V(-1).es) +
    b11Pattern(IV(-1)) +
    b11Pattern(V(-1)) + 
    b11Pattern(V(-1))
    /*
    V(-1) *3 *3 + V(-1) + V(-1) + VII(-1) +
    VII(-1) *3 *(3 + 1) +
    III *5 + VI + II *3 + IV *3 +
    (III fillSeq (_+0, _+0, _+0, _+0, _+1, _-1, _-1, _-1, _+1, _+1, _+1)) +
    III.is *3 + I *3 + IV + I *(2 + 3 +
    3) + V(-1).es *3 + V(-1) *3 + V(-1) *3
    */
  }
  
  val b12 = {
    implicit val noteDuration = (rH-)
    I(-1) * 2 +
    I(-1) * 2 +
    //
    III(-1) * 2 +
    III(-1) * 2 +
    //
    VI(-1) *2 +
    V(-2) *2 +
    //
    I(-1) + III(-1).is +
    IV(-1) *2 +
    //
//    IV(-1) + VI(-2) +
    IV(-1) + VI(-1) +
    V(-2) + V(-2)
  }
  
  val part1 = compose(s11, s12, b11, b12)
  
  val tempo = 80
  val instrument = 0
  val minScale = Minor(A)
  val majScale = Major(A)

  // Short version without repetitions
  MelodyPlayer(
    Sequential(Nil)
    + (aaCCF2 withScale minScale)
    ,
    tempo,
//    fromQN = 14*3,
//    toQN = 3*4,
    instrument = instrument
  )
}