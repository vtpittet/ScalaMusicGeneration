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
  
  
  multiRecMelody
  
  def recMelody {
    
    val scale = Minor(C)
    val tempo = 60
    
    val song = I(rS).+>(4){_ *+ (_+2, _-3, identity)} *| (_ >> (rS-))
    
    
    MelodyPlayer(song, tempo, scale)
//    Print(song, scale)
  }
  
  def multiRecMelody {
    
    val scale = Minor(C)
    val tempo = 60
    
    val song = (I() + II() + III()).+>(2)(_ /2, _ /0.5)
    MelodyPlayer(song, tempo, scale)
  }
  
  def sampleMelody {
  
    val scale = Major(C)
    val tempo = 120
  
    val song = ((I() *+ (_+1, _+2, x=>x)) * 2 *+ (_ + 2)) *| (O() * 8 + _)
  
    MelodyPlayer(song, tempo, scale)
  
  
  
    Print(song, scale)
  }
  
}