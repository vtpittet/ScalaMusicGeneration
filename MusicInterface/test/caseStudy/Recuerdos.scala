package caseStudy

import midiInterface.MelodyPlayer
import rythmics.{E => rE}
import rythmics.{H => rH}
import rythmics.{T => rT}
import segmentSystem.MusicalSegment
import segmentSystem.SequentialSegment
import tonalSystem.A
import tonalSystem.I
import tonalSystem.II
import tonalSystem.III
import tonalSystem.IV
import tonalSystem.Major
import tonalSystem.Minor
import tonalSystem.V
import tonalSystem.VI
import tonalSystem.VII
import utils.SS
import utils.Print

object Recuerdos extends App {

  
  /*
  val s1 = V() + IV() + III() + IV() + V() + V()
  
  val s2 = V() + VI() + VII() + VI() + V() + VI() + VII() + VII()
  
  val s3 = VII() + VII() + III(1) + II(1) + I(1) + II(1) + I(1) + VII().is
  * 
  */
  
  // sets duration to rH-
  def bass2(b: MusicalSegment): MusicalSegment = b.+> {_.withDuration(rH-)}
  
  // sets duration to rE and shift each note of rE
  def bass1(b: MusicalSegment): MusicalSegment = b +> {_.withDuration(rE) >> rE}
  
  def sopran2(s: MusicalSegment): MusicalSegment =
    s.+>(_ withDuration rE).+>( _ >> rE, identity).+>(_ << rE)
  
    
  def simpleSopran1(s: MusicalSegment): MusicalSegment = 
    s.+>(_.withDuration(rT) *3 >> rT)
  
  def stdSopran1(s: MusicalSegment): MusicalSegment =
    simpleSopran1(s.+>(_.withDuration(rT)*4, _.withDuration(rT)*2))
  
  def specSopran1(s: MusicalSegment): MusicalSegment =
    s.+>(_.withDuration(rT).*(2).+>(_ *3 >> rT, _ *+ (_ / (1.5), _.+(1) / (1.5), _ / (1.5)) >> rT), _.withDuration(rT).*(4).+>(_ *3 >> rT))
  
  // return composition method applying sequentially all args to the
  // melody of composed segment
  // !! side-effect sensitive, depth of param must be controoled
  def alternate(exps: MusicalSegment => MusicalSegment*): SequentialSegment => MusicalSegment = {
    val expIter = Stream.continually(exps).flatten.iterator
    (x: SequentialSegment) => SequentialSegment(x.melody.map(expIter.next()(_)))
  }
  
  def sopran1(s: SequentialSegment): MusicalSegment = alternate(stdSopran1, specSopran1)(s)
  
  
  def compose(s1: SequentialSegment, s2: MusicalSegment, b1: MusicalSegment, b2: MusicalSegment): MusicalSegment =
    sopran1(s1) | sopran2(s2) | bass1(b1) | bass2(b2)
  
  
  val m11S1 = I(1) + VII().is
  val m11S2 = V() *2
  val m11B1 = II() *3
  val m11B2 = V(-2)
  
//  val m11 = compose(m11S1, m11S2, m11B1, m11B2, true)
  
  
  
  
  
  
  
  
  val s11 = SS(SS(V(), IV(), III(), IV(), V(), V(),
    V(), VI(), VII(), VI(), V(), VI(), VII(), VII(),
    VII(), VII(), III(1), II(1), I(1), II(1)), SS(I(1), VII().is),
    SS(VII().is, VII().is, II(1).es, I(1), VII(), I(1)), SS(VII(), VI()),
    SS(VI(), VI(), V(), IV(), III(), IV()), SS(III(), II()),
    SS(II(), II()))
  
  val s12 = III() + II() + I() + II() + III() + III() +
    III() + IV() + V() + IV() + III() + IV() + V() + V() +
    V() + V() + I(1) + VII() + VI() + IV(-1) + V() *2 +
    V() *2 + V() *2 + V() + VI() + IV() *2 +
    IV() *2 + II() *2 + I() *2 + VII(-1).is *2 +
    VII(-1).is *2
    
  val b11 = V(-1) *3 *3 +
    V(-1) + V(-1) + VII(-1) + VII(-1) *3 *(3 +
    1) + III() *3 + III() *2 + VI() + II() *3 +
    IV() *3 + III().is *3 + I() *3 + IV() + I() *(2 +
    3 + 3) + V(-1).es *3 + V(-1) *3 +
    V(-1) *3
    
  val b12 = I(-1) *(3 +
    1) + III(-1) *(3 +
    1) + VI(-1) *2 + V(-2) +
    V(-2) + I(-1) + III(-1).is + IV(-1) +
    IV(-1) *2 + VI(-2) + V(-2) +
    V(-2)
  
  val part1 = compose(s11, s12, b11, b12)
  
  val s21 = SS(SS(V(), IV(), III(), IV(), V(), V(),
    V(), V(), VI(), VI(), IV(1), VI()), SS(VI(), V()),
    SS(V(), V(), I(1), I(1), VII(), IV().is), SS(VI(), V()),
    SS(V(), V(), IV(), IV(), III(), II()), SS(II(), I()))
  
  val s22 = III() + II() + I() + II() + III() + III() +
    III() + III() + IV() + IV() + II(1) + IV() + III() + III() +
    III() + III() + I() + I() + II().is + II().is + III() + III() +
    III() + III() + VI(-1).es + V(-1) + V(-1) + V(-2) + V(-1) + VI(-1)
  
  val b21 = V(-1) *3 *(3 +
    1) + VI(-1) *3 + IV() *2 + VI(-1) + IV() + V(-1) *2 + // check
    V(-1) *3 + VI(-1) + III() + VI(-1) + VII(-1) + IV().is + VII(-1) + VII(-1) *3 *(1 +
    1) + VI(-1).es + IV(-1) + VI(-1).es + V(-1) *2 + IV(-1) + V(-1) *2 + VII(-1)
  
  val b22 = I(-1) *(3 +
  4 +
  1) + VI(-2) + VII(-2) + III(-1) +
  III(-1) + II(-1) + V(-2) + I(-1)
  
  val part2 = compose(s21, s22, b21, b22)
  
  def midTrans(m: MusicalSegment): MusicalSegment = m +> {_ withDuration rE} >> rE
  
  def composeTrans(s: SequentialSegment, m: MusicalSegment, b: MusicalSegment) =
    simpleSopran1(s) | midTrans(m) | bass2(b)
  
  val sTrans22 = I() *3 + II() + III() + IV()
  val mTrans22 = V(-1) + III(-1) + V(-1) + I() + II()
  val bTrans22 = I(-1)
  
  val trans22 = composeTrans(sTrans22, mTrans22, bTrans22)
  
  val sTrans21 = I() *3 + II() + III().es + IV()
  val mTrans21 = V(-1) *2 + VII(-1) + I() + II()
  val bTrans21 = I(-1)
  
  val trans21 = composeTrans(sTrans21, mTrans21, bTrans21)
  
  def stdSopran31(s: MusicalSegment): MusicalSegment = simpleSopran1(s +> {_ *6})
  def specSopran31(s: MusicalSegment): MusicalSegment = simpleSopran1(s +> {_ *2})
  def sopran31(s: SequentialSegment): MusicalSegment = alternate(stdSopran31, specSopran31)(s)
  
  def compose3(s1: SequentialSegment, s2: MusicalSegment, b1: MusicalSegment, b2: MusicalSegment): MusicalSegment =
    sopran31(s1) | sopran2(s2) | bass1(b1) | bass2(b2)
  
  val s31 = SS(SS(
    I(), I()), SS(II(), III().es, IV()), SS(V(),
    V(), V()), SS(IV(), III(), II()), SS(I(),
    I(), I()), SS(II(), III().es, IV()), SS(V(),
    V(), VII()), SS(VI(), V(), IV()), SS(III(),
    III(), III()))
  
  val s32 =
    (V(-1) + VI(-1).es + I() + III() +
    III() + VII(-1) +> {_ *2}) + V(-1) + V(-2) + V(-1) + VI(-1) +
    (V(-1) + VI(-1).es + I() + III() +
    III() + V() +> {_ *2}) + III() + II() + I() + VI(-1) +
    (I() + II(-1).is) *2
  
  val b31 =
    (V(-1) + VI(-1).es *2 + V(-1) +
    V(-1) + VII(-1) +> {_ *3}) + VI(-1) + V(-1) + IV(-1) + V(-1) + V(-1).is *2 +
    (V(-1) + VI(-1).es *2 + V(-1) *(1 +
    3) +> {_ *3}) + V(-1) *2 + I() +
    (V(-1) + V(-2) + III(-1)) *2
  
  val b32 =
    I(-1) *(4 +
    1) + V(-2) *2 + I(-1) *(1 +
    4 +
    1) + V(-2) *2 + I(-1) *(1 +
    2)
  
  val part3 = compose3(s31, s32, b31, b32)
  
  val end = I(-1) + V(-1) + I() + III() + V() + I(1) +> {_ withDuration rE} +
    ((I(-1) | III() | I(1) | V(1)) + (I(-1) | III(-1) | V(-1) | I()) +> {_ withDuration (rH-)})
  
  val tempo = 80
  val minScale = Minor(A)
  val majScale = Major(A)
  
  // Full version
  MelodyPlayer(
    tempo,
    (part1 *2, minScale),
    (part2 + trans22 + part2 + trans21, majScale),
    (part1, minScale),
    (part2 + part3 + end, majScale)
  )
  
  
  // Short version without repetitions
  MelodyPlayer(
    tempo,
    (part1, minScale),
    (part2 + part3 + end, majScale)
  )
  
}


