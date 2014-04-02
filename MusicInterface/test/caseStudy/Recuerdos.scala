package caseStudy

import midiInterface.MelodyPlayer
import tonalSystem._
import tonalSystem.{S => tS}
import rythmics.{E => rE}
import rythmics.{H => rH}
import rythmics.{Q => rQ}
import rythmics.{T => rT}
import utils.Print
import segmentSystem.SequentialSegment
import segmentSystem.MusicalSegment
import segmentSystem.Note
import utils.SS

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
  
  
  
  
  
  
  
  
  val s1 = SS(SS(V(), IV(), III(), IV(), V(), V(),
    V(), VI(), VII(), VI(), V(), VI(), VII(), VII(),
    VII(), VII(), III(1), II(1), I(1), II(1)), SS(I(1), VII().is))
  
  val s2 = III() + II() + I() + II() + III() + III() +
    III() + IV() + V() + IV() + III() + IV() + V() + V() +
    V() + V() + I(1) + VII() + VI() + IV(-1) + V() *2
    
  val b1 = V(-1) *3 *3 +
    V(-1) + V(-1) + VII(-1) + VII(-1) *3 *(3 +
    1) + III() *3 + III() *2 + VI() + II() *3
    
  val b2 = I(-1) *4 + III(-1) *4 + VI(-1) *2 + V(-2)
  
  
  val tempo = 60
  val scale = Minor(A)
  
  /*
  val phrase1 = 
    sopran1(V() + IV() + III() + IV() + V() + V()) |
    sopran2(III() + II()+ I() + II() + III() + III()) |
    bass1(V(-1) *3 *3) |
    bass2(I(-1) * 3)
  * 
  */
    
    
  MelodyPlayer(compose(s1, s2, b1, b2), tempo, scale)
}