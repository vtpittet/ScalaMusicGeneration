package main.scala.musical

class Arpeggio(val notesStarts: List[List[Double]], val duration: Double) {
  
  def noteCount = notesStarts.size
}
