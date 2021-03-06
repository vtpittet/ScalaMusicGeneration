package utils

import segmentSystem.Note
import tonalSystem._
import segmentSystem.MusicalSegment
import segmentSystem.SequentialSegment
import segmentSystem.ParallelSegment

object PrettyPrinter {
  private val defaultIdent = "  "
  def apply(melody: MusicalSegment, scale: Scale = Major(C), ident: String = ""): String = melody match {
    case Note(tone, duration) => "" + scale.pitchTone(tone) + duration.computed
    case SequentialSegment(tracks) => {
      val tempS = tracks.foldLeft("(")(_ + apply(_, scale, ident) + ", ")
      tempS.substring(0, tempS.length-2)+ ")"
    }
    case ParallelSegment(tracks) => {
      
      tracks.foldLeft("(\n")(
            _ + ident + defaultIdent + apply(
                  _, scale, ident + defaultIdent) + '\n') +
        ident + ")"
    }
  }
}

object Print {
  def apply(melody: MusicalSegment, scale: Scale = Major(C)): Unit = println(PrettyPrinter(melody, scale))
  
}