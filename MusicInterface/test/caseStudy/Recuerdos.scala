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
  
  val tempo = 60
  val scale = Minor(A)
  
  val phrase1 = 
    sopran1(V() + IV() + III() + IV() + V() + V()) |
    sopran2(III() + II()+ I() + II() + III() + III()) |
    bass1(V(-1) *3 *3) |
    bass2(I(-1) * 3)
    
  MelodyPlayer(phrase1, tempo, scale)
}