package main.scala.musical

trait Melody {
  
  type Track = List[Chord]
  /*
  * val instrument
  * 
  */
  // beats per minute
  val tempo: Int
  
  val tracks: List[Track]
  
}