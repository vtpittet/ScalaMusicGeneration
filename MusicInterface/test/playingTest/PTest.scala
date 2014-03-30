package playingTest

import tonalSystem._
import midiInterface.MelodyPlayer
import utils.Print
import rythmics.Q
import segmentSystem.MusicalSegment
import segmentSystem.Note
import rythmics.BPM
import rythmics.T
import rythmics.{ S => rS}

object PTest extends App {
  
  val scale = Major(C)
  val tempo = 120
  
  sampleMelody
  
  def recMelody {
    val r = 4
    val song = I(rS).->(r){_ *+ (_+2, _-3, identity)} *| (_ >> (rS-))
    
    
    MelodyPlayer(song, tempo, scale)
//    Print(song, scale)
  }
  
  
  
  def sampleMelody {
  
  
    val song = ((I() *+ (_+1, _+2, x=>x)) * 2 *+ (_ + 2)) *| (O() * 8 + _)
  
    MelodyPlayer(song, tempo, scale)
  
  
  
    Print(song, scale)
  }
  
}