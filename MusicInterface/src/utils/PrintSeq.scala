package utils

import segmentSystem.SequentialSegment
import segmentSystem.ClassPredicate.isNote
import tonalSystem.Tone._
import tonalSystem.Major
import tonalSystem.C
import segmentSystem.MusicalSegment
import segmentSystem.Note

object PrintSeq {

  def apply(segment: MusicalSegment): Unit = if (validSeq(segment)) {
    val lt: ((Note, Note) => Boolean) = { (a, b) => 
      (a.tone stepsTo b.tone) > 0
    }
    
    val trueNotes = segment.notes filter { _.tone != O }
    
    val deepest = (trueNotes sortWith lt head).tone decreaseBy 1
    val highest = (trueNotes sortWith lt).reverse.head.tone
    val totalSteps = deepest stepsTo highest
    val fillChar = ".____"
    
    def alterChar(alteration: Option[Boolean]): String  = alteration match {
      case None => "\u266E"
      case Some(true) => "\u266F"
      case Some(false) => "\u266D"
    }
    
    val repr = segment.notes map { n =>
      s"${fillChar * (deepest stepsTo n.tone)}" +
      s"${alterChar(n.tone.alter)}" +
      f"${n.duration.computed}%2.2f" +
      s"${fillChar * (totalSteps - (deepest stepsTo n.tone))}"
    } mkString("\n")
    
    println(repr)
  } else println("PrintSeq: no sequential argument")
  
  private def validSeq(segment: MusicalSegment): Boolean = segment.flatAll match {
    case SequentialSegment(melody) => {
      segment.notes.filter(_.tone != O).nonEmpty &&
      (melody forall { isNote isDefinedAt _ })
    }
    case _ => false
  }
}