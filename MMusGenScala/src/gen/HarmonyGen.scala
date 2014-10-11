package gen

import segmentSystem._
import tonalSystem.Tone
import tonalSystem.Tone._

case class HarmonyGen(melody: SimpleMelody, upperBound: Note, lowerBound: Note) {
  

}

//TODO : perhaps bad structure
/* char is the characteristic tone of the degree : from 1 to 7*/
class Degree(char : Int) {
  def +(degRise: Int) : Degree = {
    return new Degree(((char - 1 + degRise)%7 + 7)%7 + 1);
  }
  
}

object Utilities {
  def toneToDegree(t : Tone) : Degree = {
    ???
  }
}

//TODO : create Degree for type of degree that is Tone modulo 8

//chord with list of notes characterized by a degree, and perhaps with other things that will be added
// (for triads and simple seventh it is ok like that)


trait CharChord {
  val tones : List[Degree] // first is the bass of the chord
  def allowedRemain(setN : List[Degree]) : List[List[Degree]]
}

case class Triad(degree : Degree) extends CharChord{ 
	//allowed sets of I, II, ... (Pitch ?)
  def allowedRemain(setN : List[Degree]) : List[List[Degree]] = {
    ???
  }
  val tones = degree :: (degree + 2) :: (degree + 4) :: Nil
  	
}

case class SeventhCh(degree : Degree) extends CharChord{
	//allowed sets of I, II, ... (Pitch ?)
  def allowedRemain(setN : List[Degree]) : List[List[Degree]] = {
    ???
  }
  val tones = degree :: (degree + 2) :: (degree + 4) :: (degree + 6) :: Nil
}

case class Chord4(notes : List[Tone], template : CharChord){
  
}


