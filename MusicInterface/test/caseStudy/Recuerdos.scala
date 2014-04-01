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
  
  def sopran1(s: MusicalSegment): MusicalSegment =
    s.+>(_.withDuration(rT)*4, _.withDuration(rT)*2).+>(_ * 3 >> rT)

  
  def compose(s1: MusicalSegment, s2: MusicalSegment, b1: MusicalSegment, b2: MusicalSegment): MusicalSegment =
    sopran1(s1) | sopran2(s2) | bass1(b1) | bass2(b2)
  
  val s1 = V() + IV() + III() + IV() + V() + V() +
    V() + VI() + VII() + VI() + V() + VI() + VII() + VII() +
    VII() + VII() + III(1) + II(1) + I(1) + II(1)
  
  val s2 = III() + II() + I() + II() + III() + III() +
    III() + IV() + V() + IV() + III() + IV() + V() + V() +
    V() + V() + I(1) + VII() + VI() + IV(-1)
    
  val b1 = V(-1) *3 *3 +
    V(-1) + V(-1) + VII(-1) + VII(-1) *3 *(3 +
    1) + III() *3 + III() *2 + VI()
    
  val b2 = I(-1) *4 + III(-1) *4 + VI(-1) *2
  
  
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