package main.scala.musical

case class Arpeggio(val notesStarts: List[List[Double]], val duration: Double) {
  
  def noteCount = notesStarts.size
}
