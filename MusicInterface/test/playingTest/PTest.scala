package playingTest

import tonalSystem._
import midiInterface.MelodyPlayer
import utils.Print

object PTest extends App {
  
  val song = ((I() *+ (_+1, _+2, x=>x)) * 2 *+ (_ + 2)) *| (O() * 8 + _)
  
  val scale = Major(C)
  
  val tempo = 180
  
  MelodyPlayer(song, tempo, scale)
  
  Print(song)
}