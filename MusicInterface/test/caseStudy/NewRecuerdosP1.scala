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
  
  def newPhraseT1(scale: Scale, ts: Int, tb: Int): MS = {
    val m1 = newMakeMesureT1(scale, 0, 0)
    val m2 = newMakeMesureT1(scale, ts, tb)
    m1 + m2
  }
  
  def newPhraseRetT1(scale: Scale, ts: Int, tb: Int): MS = {
    val m1 = newMakeMesureT1(scale, -1, 0)
    val m2 = newMakeMesureT1ret(scale, ts, tb)
    m1 + m2
  }
  
  def makeMesureT2(s1: Tone, s2: Tone, b1: Tone, b2: Tone): MS = {
    val fs1 = {
      implicit val noteDuration = rT
      (s1 + 1) *2 + (s1 *4) mapIf (isNote
        thenDo ({n => O + (n fillSeq (_ / (1.5), _ / (1.5) + 1, _ / (1.5)))}, 1, 1, 2)
        orDo (O + _ *3))
    }
    val fs2b1 = {
      implicit val noteDuration = rE
      O + b1 + s2 + b1 + s2 + b1
    }
    val fb2 = b2(rH-)
    fs1 | fs2b1 | fb2
  }
  
  def newMakeMesureT2(scale: Scale): MS = {
    makeMesureT2(III, I, V(-1), I(-1)) withScale scale
  }
  
  def newMakeMesureSensiT1(scale: Scale): MS = {
    makeMesureT1(III, I, VII(-1), I(-1), 0, 0) withScale scale
  }
  
  def newPhraseAppoSensi(scale: Scale): MS = {
    newMakeMesureT2(scale) + newMakeMesureSensiT1(scale)
  }
  
  
  val aaCCF = {
    phraseT1(I(-1), true, 1, 0) +
    phraseT1(I(-1), false, 1, 1) +
    phraseT1(III(-1), true, 1, 0) +
    phraseT1(III(-1), false, 0, 0) +
    phraseT1(VI(-1), true, 1, 1)
  }
  
  val aaCCFE2 = {
//    newPhraseRetT1(Minor(A), 1, 0) +
//    newPhraseT1(Minor(A), 1, 1) +
//    newPhraseRetT1(Major(C), 1, 0) +
//    newPhraseT1(Major(C), 0, 0) +
//    newPhraseRetT1(Major(F), 1, 1) +
    newPhraseAppoSensi(Major(E))
  }
  
  val tempo = 80
  val instrument = 0
  val minScale = Minor(A)
  val majScale = Major(A)

  // Short version without repetitions
  MelodyPlayer(
    Sequential(Nil)
    + (aaCCFE2)
    ,
    tempo,
//    fromQN = 14*3,
//    toQN = 3*4,
    instrument = instrument
  )
}