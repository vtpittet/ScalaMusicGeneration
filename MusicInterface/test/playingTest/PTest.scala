package playingTest

import tonalSystem._
import tonalSystem.Tone._
import midiInterface.MelodyPlayer
import utils.Print
import rythmics.Q
import segmentSystem.MusicalSegment
import segmentSystem.Note
import rythmics.BPM
import rythmics.T
import rythmics.{ S => rS}
import rythmics.{ E => rE}
import utils.MelodyWriter
import utils.PrintSeq
import segmentSystem.ClassPredicate.isNote

object PTest extends App with MelodyWriter {
  
  
//  recMelody
//  multiRecMelody
  sampleMelody
  
  def recMelody {
    
    implicit val scale = Minor(C)
    val tempo = 60
    
    val song = I(rS).appN(4) { s =>
      s mapNotes (_ fillSeq (_+2, _-3, identity))
    } fillPar (O(rS-) + _)
    
    
    MelodyPlayer(song, tempo)
//    Print(song, scale)
  }
  
  def multiRecMelody {
    
    implicit val scale = Minor(C)
    val tempo = 60
    
    val song = (I + II + III).appN(2) { s =>
      s mapNotes (_ /2, _ /0.5)
    }
    MelodyPlayer(song, tempo)
  }
  
  def sampleMelody {
  
    implicit val scale = Major(C)
    val tempo = 120
  
    implicit val noteDuration = rE
    val song = ((I fillSeq (_ + 1, _ + 2, identity)) *2 fillSeq (_ + 2)) fillPar (O *8 + _)
  
    MelodyPlayer(song, tempo)
  
  
  
    println(song)
  }
  
}