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
import segmentSystem.EmptySeq

object NewRecuerdosP1 extends App with MelodyWriter {
  
  abstract class Phrase {
    import scala.math.random
    
    val scale: Scale
    val transition: Int
    val transRange: List[Int]
    val stepRange: List[Int] = List(-1, 0, 1)
    
    val startingDegree: Int
    val endingDegree: Int
    val pMaker: ((Scale, Int) => MS)
    
    lazy val actualTrans = if (transRange contains transition) transition else 0
    
    def getP: MusicalSegment = pMaker(scale, transition)
    
    private def nextP(
      pBuilder: (Scale, Int) => Phrase,
      pStartingDegree: Int,
      major: Boolean,
      trans: Int, step: Int) = {
      
      val actualTrans = if (transRange contains transition) transition else 0
      val toScale = if (major) Major(_) else Minor(_)
      val nextPitch = scale.tonic + endingDegree + actualTrans + step - pStartingDegree
      pBuilder(toScale(nextPitch.asInstanceOf[Pitch with Tonality]), trans)
    }
    
    def nextBaseP(major: Boolean, trans: Int, step: Int): Phrase = {
      nextP(BaseP(_, _), 4, major, trans, step)
    }
    
    def nextRetP(major: Boolean, trans: Int, step: Int): Phrase = {
      nextP(RetP(_, _), 4, major, trans, step)
    }
    
    def nextAppoP(major: Boolean, trans: Int, step: Int): Phrase = {
      nextP(AppoP(_, _), 3, major, trans, step)
    }
    
    def nextLateP(major: Boolean, trans: Int, step: Int): Phrase = {
      nextP(LateP(_, _), 8, major, trans, step)
    }
    
    def next: Phrase = {
      val nextBuilder = (random * 4).toInt match {
        case x if (x == 0) => nextBaseP(_, _, _)
        case x if (x == 1) => nextRetP(_, _, _)
        case x if (x == 2) => nextAppoP(_, _, _)
        case x if (x == 3) => nextLateP(_, _, _)
      }
      val major = true //(random * 2).toInt == 0
      val trans = 1 - (random * 3).toInt
      val step = 1 - (random * 3).toInt
      
      nextBuilder(major, trans, step)
    }
    
    def nextC: Phrase = {
      val majTones = List(A, C, E, F)
      val minTones = List(A, D)
      
      val nextTrans = transRange((scala.math.random * transRange.size).toInt)
      
      val pTypes = List(
        (BaseP.startingDegree, BaseP(_, _)),
        (RetP.startingDegree, RetP(_, _)),
        (AppoP.startingDegree, AppoP(_, _)),
        (LateP.startingDegree, LateP(_, _)))
      
      val possibleTrans = for {
        s <- stepRange
        p <- pTypes
      } yield (p._2, scale.tonic + endingDegree + actualTrans + s - p._1)
      
      val majorTrans = possibleTrans filter { majTones contains _._2.newSelf(0, 0) } map { t =>
        (t._1(Major(t._2.asInstanceOf[Pitch with Tonality]), nextTrans))
      }
      val minorTrans = possibleTrans filter { minTones contains _._2.newSelf(0, 0) } map { t =>
        (t._1(Minor(t._2.asInstanceOf[Pitch with Tonality]), nextTrans))
      }
      val nexts = minorTrans ::: majorTrans
      
      val next = nexts((scala.math.random * nexts.size).toInt)
      println(next)
      next
    }
  }
  
  case class BaseP(scale: Scale, transition: Int) extends Phrase {
    val startingDegree = 4
    val endingDegree = 4
    val transRange = List(-1, 0, 1)
    val pMaker = { (s: Scale, t: Int) => baseP(s, t, t) }
  }
  object BaseP extends BaseP(Minor(A), 0)
  
  case class RetP(scale: Scale, transition: Int) extends Phrase {
    val startingDegree = 4
    val endingDegree = 3
    val transRange = List(-1, 0, 1)
    val pMaker = { (s: Scale, t: Int) => retBaseP(s, t, t)}
  }
  object RetP extends RetP(Minor(A), 0)
  
  case class AppoP(scale: Scale, transition: Int) extends Phrase {
    val startingDegree = 3
    val endingDegree = 2
    val transRange = List(-1, 0, 1)
    val pMaker = { (s: Scale, t: Int) => appoSensiP(s, t, t)}
  }
  object AppoP extends AppoP(Minor(A), 0)
  
  case class LateP(scale: Scale, transition: Int) extends Phrase {
    val startingDegree = 8
    val endingDegree = 7
    val transRange = List(0)
    val pMaker = { (s: Scale, t: Int) => lateResP(s, t, t)}
  }
  object LateP extends LateP(Minor(A), 0)
  
  
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
    val octaveJump = if (List(D, E, F, G) contains scale.tonic) -1 else 0
    appoM(scale, 0, 0) + leadBaseM(scale, ts, tb) //+ (octaveJump * 7)
  }
  
  def makeLateResM(s1: Tone, s2: Tone, b1: Tone, b2: Tone, res: Int, tb: Int): MS = {
    makeBaseM(s1 decreaseBy res, s2, b1, b2, res, tb)
  }
  
  def lateResM(scale: Scale): MS = {
    makeLateResM(I(1), V, III, I(-1), -1, 0) withScale scale
  }
  
  def retLateResM(scale: Scale, tb: Int): MS = {
    makeLateResM(I(1), V, I, III(-1), 1, tb) withScale scale
  }
  
  def lateResP(scale: Scale, ts: Int, tb: Int): MS = {
    val octaveJump = if (List(D, E, F, G) contains scale.tonic) -1 else 0
    lateResM(scale) + retLateResM(scale, tb) //+ (octaveJump * 7)
  }
  
  val part1 = {
    retBaseP(Minor(A), 1, 0) +
    baseP(Minor(A), 1, 1) +
    retBaseP(Major(C), 1, 0) +
    baseP(Major(C), 0, 0) +
    retBaseP(Major(F), 1, 1) +
    appoSensiP(Major(E), 0, 0) +
    lateResP(Major(A), 0, 0) +
    appoSensiP(Minor(D), 0, 0) +
    (lateResP(Minor(D), 0, 0) - 7) +
    (appoSensiP(Major(E), 0, 0) - 7)
  }
  
  
  def getRndP: MS = {
    def rnd = scala.math.random
    val majTones = List (A, C, E, F)
    val minTones = List (A, D)
    
    
    val allowedScales = (majTones map { Major(_) }) ::: (minTones map { Minor(_) })
    val phrase = rnd * 4.0 match {
      case x if (x < 1) => retBaseP(_, _, _)
      case x if (x < 2) => baseP(_, _, _)
      case x if (x < 3) => appoSensiP(_, _, _)
      case _ => lateResP(_, _, _)
    }
    
    val scale = allowedScales((rnd * allowedScales.size).toInt)
    
    val ts = 1 - (rnd * 3).toInt
    val tb = 1 - (rnd * 3).toInt
    
    phrase(scale, ts, tb)
  }
  
  def rndPart(size: Int) = O.fillSeq(size){ x =>
    getRndP
  }
  
  def rndLinkedPart(init: Phrase, size: Int) = {
    def phrases(phrase: Phrase): Stream[Phrase] = phrase #:: phrases(phrase.next)
    phrases(init) take size map { _.getP } reduceLeft { _ + _ }
  }
  
  def rndLinkedCPart(init: Phrase, size: Int) = {
    def phrases(phrase: Phrase): Stream[Phrase] = phrase #:: phrases(phrase.nextC)
    phrases(init) take size map { _.getP } reduceLeft { _ + _ }
  }
  
  val tempo = 80
  val instrument = 0

  // Short version without repetitions
  MelodyPlayer(
    Sequential(Nil)
    + (part1)
//    + rndPart(7)
//    + rndLinkedPart(RetP(Minor(A), 1), 7)
//    + rndLinkedCPart(RetP(Minor(A), 1), 7)
    ,
    tempo,
//    fromQN = 14*3,
//    toQN = 3*4,
    instrument = instrument
  )
}