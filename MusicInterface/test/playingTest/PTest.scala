package playingTest

import tonalSystem._
import midiInterface.MelodyPlayer
import utils.Print

object PTest extends App {
  
  val song = (I() + II() + III() + I()) *+ (_ + 2) *| (O() * 4 + _)
  
  val scale = Major(C)
  
  val tempo = 120
  
  MelodyPlayer(song, tempo, scale).play
  
  Print(song)
}