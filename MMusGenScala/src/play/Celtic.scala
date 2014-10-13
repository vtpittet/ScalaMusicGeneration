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

object Celtic extends App with MelodyWriter {
  implicit val noteDuration = E

  val part1 = {
    ( O + I + II 
	+ III(/(2/3)) + II(/2) + I 
	+ I + (VI + V 
	+ V(/2) + VI(/(2/3)))(-7) + I 
	+ II(Q) + I(S) + I(S) 
	+ III(/(2/3)) + IV(/2) + V 
	+ III + I + (VII 
	+ VI(/(1/4)))(-7)) -2
  }
  
  val part1harm = {
	( O 
	+ III(/(2/3)) 
	+ I 
	+ VI(/(2/3))(-7) 
	+ II(Q) 
	+ III(/(2/3))
	+ III
	+ VI(/(1/4))(-7)) (/(2/3)) -2
  }
  
  val part2 = {
    ( III + IV
	+ V(/(2/3)) + IV(/2) + III
	+ III + II + I 
	+ VI(/2)(-7) + II(/(2/3)) + III 
	+ II(Q) + III(S) + IV(S) 
	+ V(/(2/3)) + III(/2) + I
	+ I + III + V 
	+ VI(/(1/4))) -2
  }
  
  val part2harm = {
	( VI
	+ V(/(2/3))
	+ III 
	+ II(/(2/3)) 
	+ II(Q) 
	+ V(/(2/3))
	+ I
	+ VI(/(1/4))) (/(2/3)) -2
  }
  
  val part3 = {
    ( I(+7) + VI
	+ V(/(2/3)) + III(/2) + II
	+ I + II + III 
	+ VI(/2)(-7) + II(/(2/3)) + III 
	+ II(Q) + I(S) + II(S) 
	+ III(/(2/3)) + II(/2) + I
	+ I + (VI + V 
	+ VI(/(1/4)))(-7)) -2
  }
  
  val part3harm = {
	( VI
	+ V(/(2/3))
	+ I  
	+ II(/(2/3)) 
	+ II(Q) 
	+ III(/(2/3))
	+ I 
	+ VI(/(1/4))(-7)) (/(2/3)) -2
  }


  def compose(p1 : MusicalSegment, p2 : MusicalSegment, p3: MusicalSegment ): MusicalSegment =
    (p1 + p2 + p3)

  val part = compose(part1, part2, part3) - 7 //voir pour le -7

  val tempo = 60
  val instrument = 0
  val minScale = Minor(A)

  MelodyPlayer(
    Sequential(Nil)
      + (part withScale minScale),
    tempo,
    instrument = instrument)

}
