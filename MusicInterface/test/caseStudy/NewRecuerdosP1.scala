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
  
  
  def makeBaseM(s1: Tone, s2: Tone, b1: Tone, b2: Tone, ts: Int, tb: Int): MS = {
    
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
  
  def baseM(scale: Scale, ts: Int, tb: Int): MS = {
    makeBaseM(V, III, V(-1), I(-1), ts, tb) withScale scale
  }
  
  def retBaseM(scale: Scale, ts: Int, tb: Int): MS = {
    makeBaseM(III, I, V(-1), I(-1), ts, tb) withScale scale
  }
  
  def baseP(scale: Scale, ts: Int, tb: Int): MS = {
    val m1 = baseM(scale, 0, 0)
    val m2 = baseM(scale, ts, tb)
    m1 + m2
  }
  
  def retBaseP(scale: Scale, ts: Int, tb: Int): MS = {
    val m1 = baseM(scale, -1, 0)
    val m2 = retBaseM(scale, ts, tb)
    m1 + m2
  }
  
  def makeAppoM(s1: Tone, s2: Tone, b1: Tone, b2: Tone, ts: Int, tb: Int): MS = {
    val fs1 = {
      implicit val noteDuration = rT
      (s1 + 1) *2 + (s1 *3) + (s1 + ts) mapIf (isNote
        thenDo ({n => O + (n fillSeq (_ / (1.5), _ / (1.5) + 1, _ / (1.5)))}, 1, 1, 2)
        orDo (O + _ *3))
    }
    val fs2b1 = {
      implicit val noteDuration = rE
      O + b1 + s2 + b1 + (s2 + ts) + (b1 + tb)
    }
    val fb2 = b2(rH-)
    fs1 | fs2b1 | fb2
  }
  
  def appoM(scale: Scale, ts: Int, tb: Int): MS = {
    makeAppoM(III, I, V(-1), I(-1), ts, tb) withScale scale
  }
  
  def leadBaseM(scale: Scale, ts: Int, tb: Int): MS = {
    makeBaseM(III, I, VII(-1), I(-1), ts, tb) withScale scale
  }
  
  def appoSensiP(scale: Scale, ts: Int, tb: Int): MS = {
    appoM(scale, 0, 0) + leadBaseM(scale, ts, tb)
  }
  
  def makeLateResM(s1: Tone, s2: Tone, b1: Tone, b2: Tone, res: Int): MS = {
    makeBaseM(s1 decreaseBy res, s2, b1, b2, res, 0)
  }
  
  def lateResM(scale: Scale): MS = {
    makeLateResM(I(1), V, III, I(-1), -1) withScale scale
  }
  
  def retLateResM(scale: Scale): MS = {
    makeLateResM(I(1), V, I, III(-1), 1) withScale scale
  }
  
  def lateResP(scale: Scale): MS = {
    lateResM(scale) + retLateResM(scale)
  }
  
  val part1 = {
    retBaseP(Minor(A), 1, 0) +
    baseP(Minor(A), 1, 1) +
    retBaseP(Major(C), 1, 0) +
    baseP(Major(C), 0, 0) +
    retBaseP(Major(F), 1, 1) +
    appoSensiP(Major(E), 0, 0) +
    lateResP(Major(A)) +
    appoSensiP(Minor(D), 0, 0) +
    (lateResP(Minor(D)) - 7) +
    (appoSensiP(Major(E), 0, 0) - 7)
  }
  
  val tempo = 120
  val instrument = 0
  val minScale = Minor(A)
  val majScale = Major(A)

  // Short version without repetitions
  MelodyPlayer(
    Sequential(Nil)
    + (part1)
    ,
    tempo,
//    fromQN = 14*3,
//    toQN = 3*4,
    instrument = instrument
  )
}