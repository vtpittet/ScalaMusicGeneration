package main.scala

import main.scala.musical.Arpeggio
import main.scala.musical._
import main.scala.musical.Tone
import main.scala.melody.Cadence
import main.scala.midiInterface.MelodyReader
import main.scala.musical.AS

object CadenceRunner {
  def main(args: Array[String]) {
    val tonic: Tone = A(0)
    
    val arpeggio0: Arpeggio = Arpeggio(List(List(0), List(1/4.0, 3/4.0), List(2/4.0)), 1)
    val arpeggio1: Arpeggio = Arpeggio(List(0::Nil, (3/8.0)::(7/8.0)::Nil, (4/8.0)::Nil), 1)
    val arpeggio2: Arpeggio = Arpeggio(List(List(0, 1, 2),
          List(1/3.0, 2/3.0, 4/3.0, 5/3.0, 7/3.0, 8/3.0),
          List(1/3.0, 2/3.0, 4/3.0, 5/3.0, 7/3.0, 8/3.0)), 3)
    
    
    val tempo: Int = 80
    
    MelodyReader(Cadence(tonic, arpeggio0, tempo))
    
    val currTime = System.currentTimeMillis()
    while(System.currentTimeMillis()-currTime < 1000) {}
  }
}