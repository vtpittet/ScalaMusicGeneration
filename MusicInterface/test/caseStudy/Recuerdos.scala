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
  
  def stdSopran1(s: MusicalSegment): MusicalSegment =
    s.+>(_.withDuration(rT)*4, _.withDuration(rT)*2).+>(_ * 3 >> rT)
  
  def specSopran1(s: MusicalSegment): MusicalSegment =
    s.+>(_.withDuration(rT).*(2).+>(_ *3 >> rT, _ *+ (_ / (1.5), _.+(1) / (1.5), _ / (1.5)) >> rT), _.withDuration(rT).*(4).+>(_ *3 >> rT))
  
  // Alternate sopran1 composition for each melody of given sequentialSegment
  // !! side-effect sensitive, depth of param must be controoled
  def sopran1(s: SequentialSegment): MusicalSegment = {
    val sIter = Stream.continually(List(stdSopran1(_), specSopran1(_))).flatten.iterator
    SequentialSegment(s.melody.map(sIter.next()(_)))
  }
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
  
  
  
  
  
  
  val tempo = 80
  val minScale = Minor(A)
  val majScale = Major(A)
  
  /*
  val phrase1 = 
    sopran1(V() + IV() + III() + IV() + V() + V()) |
    sopran2(III() + II()+ I() + II() + III() + III()) |
    bass1(V(-1) *3 *3) |
    bass2(I(-1) * 3)
  * 
  */
  
  
    
  MelodyPlayer(part1, tempo, minScale)
  MelodyPlayer(part2, tempo, majScale)
}


