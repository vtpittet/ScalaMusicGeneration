package caseStudy

import midiInterface.MelodyPlayer
import rythmics.{E => rE}
import rythmics.{H => rH}
import rythmics.{T => rT}
import segmentSystem.MusicalSegment
import segmentSystem.SequentialSegment
import tonalSystem.A
import tonalSystem.Tone._
import tonalSystem.Major
import tonalSystem.Minor
import utils.SS
import utils.Print
import utils.PrettyPrinter
import utils.MelodyWriter
import segmentSystem.ClassPredicate.isSeq
import segmentSystem.Sequential
import segmentSystem.ClassPredicate
import segmentSystem.ClassPredicate.isNote

object Recuerdos extends App with MelodyWriter {
  
  
  // sets duration to rH-
  def bass2(b: MusicalSegment): MusicalSegment = b.+> {_.withDuration(rH-)}
  
  // sets duration to rE and shift each note of rE
  def bass1(b: MusicalSegment): MusicalSegment = b +> {O(rE) + _.withDuration(rE)}
  
  def sopran2(s: MusicalSegment): MusicalSegment =
    s.+>(_ withDuration rE).+>(O(rE) + _, identity).+>(_ + O(rE))
  
    
  def simpleSopran1(s: MusicalSegment): MusicalSegment = 
    s.+>(O(rT) + _.withDuration(rT) *3)
  
  def stdSopran1(s: MusicalSegment): MusicalSegment =
    simpleSopran1(s.+>(_.withDuration(rT)*4, _.withDuration(rT)*2))
  
  def specSopran1(s: MusicalSegment): MusicalSegment =
    s.+>(_.withDuration(rT).*(2).+>(O(rT) + _ *3,O(rT) + _ *+ (_ / (1.5), _.+(1) / (1.5), _ / (1.5))), _.withDuration(rT).*(4).+>(O(rT) + _ *3))
  
  // return composition method applying sequentially all args to the
  // melody of composed segment
  // !! side-effect sensitive, depth of param must be controlled
  def alternate(exps: MusicalSegment => MusicalSegment*): SequentialSegment => MusicalSegment = {
    _ expand (isSeq given (_.height==1), exps:_*)
  }
  
  def sopran1(s: SequentialSegment): MusicalSegment = {
    alternate(stdSopran1, specSopran1)(s)
  }
  
  
  def compose(s1: SequentialSegment, s2: MusicalSegment, b1: MusicalSegment, b2: MusicalSegment): MusicalSegment =
    sopran1(s1) | sopran2(s2) | bass1(b1) | bass2(b2)
  
  val s11 = {
    (V + IV + III + IV + V + V + V + VI +
    VII + VI + V + VI + VII + VII + VII + VII +
    III(1) + II(1) + I(1) + II(1)) ++ (I(1) + VII/*.is*/) + (VII/*.is*/ + VII/*.is*/ +
    II(1)/*.es*/ + I(1) + VII + I(1)) + (VII + VI) + (VI + VI +
    V + IV + III + IV) + (III + II) + (II + II)
  }
    
  val s12 =
    III + II + I + II + III + III + III + IV +
    V + IV + III + IV + V + V + V + V +
    I(1) + VII + VI + IV(-1) + V *2 + V *2 +
    V *2 + V + VI + IV *2 + IV *2 +
    II *2 + I *2 + VII(-1).is *2 + VII(-1).is *2
    
  val b11 =
    V(-1) *3 *3 + V(-1) + V(-1) + VII(-1) +
    VII(-1) *3 *(3 + 1) +
    III *3 + III *2 + VI + II *3 + IV *3 +
    III.is *3 + I *3 + IV + I *(2 + 3 +
    3) + V(-1).es *3 + V(-1) *3 + V(-1) *3
    
  val b12 =
    I(-1) *(3 + 1) +
    III(-1) *(3 + 1) +
    VI(-1) *2 + V(-2) + V(-2) +
    I(-1) + III(-1).is + IV(-1) + IV(-1) +
    IV(-1) + VI(-2) + V(-2) + V(-2)
  
  val part1 = compose(s11, s12, b11, b12)
  
  val s21 = (V + IV + III + IV + V + V +
    V + V + VI + VI + IV(1) + VI) ++ (VI + V) +
    (V + V + I(1) + I(1) + VII + IV.is) + (VI + V) +
    (V + V + IV + IV + III + II) + (II + I)
  
  val s22 = III + II + I + II + III + III +
    III + III + IV + IV + II(1) + IV + III + III +
    III + III + I + I + II.is + II.is + III + III +
    III + III + VI(-1).es + V(-1) + V(-1) + V(-2) + V(-1) + VI(-1)
  
  val b21 = V(-1) *3 *(3 +
    1) + VI(-1) *3 + IV *2 + VI(-1) + IV + V(-1) *2 + // check
    V(-1) *3 + VI(-1) + III + VI(-1) + VII(-1) + IV.is + VII(-1) + VII(-1) *3 *(1 +
    1) + VI(-1).es + IV(-1) + VI(-1).es + V(-1) *2 + IV(-1) + V(-1) *2 + VII(-1)
  
  val b22 = I(-1) *(3 +
  4 +
  1) + VI(-2) + VII(-2) + III(-1) +
  III(-1) + II(-1) + V(-2) + I(-1)
  
  val part2 = compose(s21, s22, b21, b22)
  
  def midTrans(m: MusicalSegment): MusicalSegment = O(rE) + (m +> {_ withDuration rE})
  
  def composeTrans(s: SequentialSegment, m: MusicalSegment, b: MusicalSegment) =
    simpleSopran1(s) | midTrans(m) | bass2(b)
  
  val sTrans22 = I *3 + II + III + IV
  val mTrans22 = V(-1) + III(-1) + V(-1) + I + II
  val bTrans22 = I(-1)
  
  val trans22 = composeTrans(sTrans22, mTrans22, bTrans22)
  
  val sTrans21 = I *3 + II + III.es + IV
  val mTrans21 = V(-1) *2 + VII(-1) + I + II
  val bTrans21 = I(-1)
  
  val trans21 = composeTrans(sTrans21, mTrans21, bTrans21)
  
  def stdSopran31(s: MusicalSegment): MusicalSegment = simpleSopran1(s +> {_ *6})
  def specSopran31(s: MusicalSegment): MusicalSegment = simpleSopran1(s +> {_ *2})
  def sopran31(s: SequentialSegment): MusicalSegment = alternate(stdSopran31, specSopran31)(s)
  
  def compose3(s1: SequentialSegment, s2: MusicalSegment, b1: MusicalSegment, b2: MusicalSegment): MusicalSegment =
    sopran31(s1) | sopran2(s2) | bass1(b1) | bass2(b2)
  
  val s31 = (
    I + I) ++ (II + III.es + IV) + (V +
    V + V) + (IV + III + II) + (I +
    I + I) + (II + III.es + IV) + (V +
    V + VII) + (VI + V + IV) + (III +
    III + III)
  
  val s32 =
    (V(-1) + VI(-1).es + I + III +
    III + VII(-1) +> {_ *2}) + V(-1) + V(-2) + V(-1) + VI(-1) +
    (V(-1) + VI(-1).es + I + III +
    III + V +> {_ *2}) + III + II + I + VI(-1) +
    (I + II(-1).is) *2
  
  val b31 =
    (V(-1) + VI(-1).es *2 + V(-1) +
    V(-1) + VII(-1) +> {_ *3}) + VI(-1) + V(-1) + IV(-1) + V(-1) + V(-1).is *2 +
    (V(-1) + VI(-1).es *2 + V(-1) *(1 +
    3) +> {_ *3}) + V(-1) *2 + I +
    (V(-1) + V(-2) + III(-1)) *2
  
  val b32 =
    I(-1) *(4 +
    1) + V(-2) *2 + I(-1) *(1 +
    4 +
    1) + V(-2) *2 + I(-1) *(1 +
    2)
  
  val part3 = compose3(s31, s32, b31, b32)
  
  val end = I(-1) + V(-1) + I + III + V + I(1) +> {_ withDuration rE} +
    ((I(-1) | III | I(1) | V(1)) + (I(-1) | III(-1) | V(-1) | I) +> {_ withDuration (rH-)})
  
  val tempo = 200
  val instrument = 0
  val minScale = Minor(A)
  val majScale = Major(A)

  // Short version without repetitions
  MelodyPlayer(
    Sequential(Nil)
    + (part1 withScale minScale)
    + (part2 withScale majScale)
    + (part3 withScale majScale)
    + (end withScale majScale)
    ,
    tempo,
//    fromQN = 3*15,
//    toQN = 3*15,
    instrument = instrument
  )
  
//   Full version
//  MelodyPlayer(
//    (part1 *2 withScale minScale)
//    + (part2 + trans22 + part2 + trans21 withScale majScale)
//    + (part1 withScale minScale)
//    + (part2 + part3 + end withScale majScale),
//    tempo
//  )
  
  
}


