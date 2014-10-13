package play

import midiInterface.MelodyPlayer
import rythmics._
import rythmics.{ E => rE }
import rythmics.{ H => rH }
import rythmics.{ T => rT }
import segmentSystem.MusicalSegment
import segmentSystem.SequentialSegment
import tonalSystem.G
import tonalSystem.Tone._
import tonalSystem.Major
import tonalSystem.Minor
import utils.SS
import utils.Print
import utils.PrettyPrinter
import utils.MelodyWriter
import segmentSystem.Sequential
import segmentSystem.ClassPredicate
import gen._

object Celtic extends App with MelodyWriter {
  implicit val noteDuration = E

  val part1 = {
    ( O + III + IV
	+ V/(2/3.0) + IV/(2.0) + III
	+ III + I + (VII -7) 
	+ (VII-7)/2.0 + I/(2/3.0) + III
	+ IV(Q) + III(S) + IV(S) 
	+ V/(2/3.0) + VI/(2.0) + VII 
	+ V + III + II 
	+ I/(1/4.0))
  }
  
  val part1harm = {
	( O 
	+ V 
	+ III 
	+ I
	+ IV 
	+ V
	+ V
	+ I/(1/2.0)) /(1/3.0)
  }
  
  val part2 = {
    ( V + VI
	+ VII/(2/3.0) + VI/2.0 + V
	+ V + IV + III 
	+ I/2.0 + IV/(2/3.0) + V 
	+ IV(Q) + V(S) + VI(S) 
	+ VII/(2/3.0) + V/2.0 + III
	+ III + V + VII 
	+ ((I + 7)/(1/4.0)))
  }
  
  val part2harm = {
	( VII
	+ V 
	+ IV 
	+ IV
	+ VII
	+ III
	+ I/(1/2.0) +7) /(1/3.0)
  }
  
  val part3 = {
    ( (III +7) + (I+7) //TODO : pq qd III(+7) et I(+7) pb en VII(-7) aussi ?
	+ VII/(2/3.0) + V/2.0 + IV
	+ III + IV + V 
	+ I/2.0 + IV/(2/3.0) + V 
	+ IV(Q) + III(S) + IV(S) 
	+ V/(2/3.0) + IV/2.0 + III
	+ III + I + (VII -7)
	+ I/(1/4.0))
  }
  
  val part3harm = {
	( VII
	+ III  
	+ IV 
	+ IV 
	+ V
	+ III 
	+ I/(1/2.0)) /(1/3.0)
  }


  def compose(p1 : MusicalSegment, p2 : MusicalSegment, p3: MusicalSegment, i: Int ): MusicalSegment =
    (p1 + p2 + p3) + i

  val part = (compose(part1, part2, part3, 14) ) | compose(part1harm, part2harm, part3harm, 7)

  val tempo = 60
  val instrument = 0
  val minScale = Minor(G) //TODO : pq A marche pas ?

  MelodyPlayer(
    Sequential(Nil)
      + (part withScale minScale),
    tempo,
    instrument = instrument)
    
  val d = HarmonyGen(compose(part1harm, part2harm, part3harm, 14))
  val e = compose(part1, part2, part3, 14) | d.harmonize._2
  
  MelodyPlayer(
    Sequential(Nil)
      + (e withScale minScale),
    tempo,
    instrument = instrument)

}
