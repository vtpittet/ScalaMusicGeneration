package caseStudy

import midiInterface.MelodyPlayer
import tonalSystem._
import tonalSystem.{S => tS}
import rythmics.{E => rE}
import rythmics.{H => rH}
import rythmics.{Q => rQ}
import utils.Print

object Recuerdos extends App {

  
  /*
  val s1 = V() + IV() + III() + IV() + V() + V()
  
  val s2 = V() + VI() + VII() + VI() + V() + VI() + VII() + VII()
  
  val s3 = VII() + VII() + III(1) + II(1) + I(1) + II(1) + I(1) + VII().is
  * 
  */
  
  
  val tempo = 60
  val scale = Minor(A)
  
  
  val phrase1 =
    (V() + IV() + III() + IV() + V() + V()).+>(_*4, _*2).+>(_ * 3 >> rQ)/8 |
    (III() + II()+ I() + II() + III() + III()).+>(_ >> rQ, identity).+>(n => n + O()) /2 |
    (V(rE, -1) + O(rE)) * 3 * 3 >> rE |
    I(rH-, -1) * 3
    
    
//  Print((V() + IV() + III() + IV() + V() + V()).+>(_*2, identity))
  MelodyPlayer(phrase1, tempo, scale)
}