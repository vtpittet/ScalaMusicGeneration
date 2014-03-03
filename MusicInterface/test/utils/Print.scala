package utils

import segmentSystem.Note
import tonalSystem._
import segmentSystem.MusicalSegment
import segmentSystem.SequentialSegment
import segmentSystem.ParallelSegment

object Print {
  def apply(melody: MusicalSegment, scale: Scale = Major(C(0))): Unit = prettyPrint(melody, "", scale)
  
  private val defaultIdent = "  "
  private def prettyPrint(melody: MusicalSegment, ident: String, scale: Scale): Unit = melody match {
    case Note(tone, duration) => print(scale.pitchTone(tone) + "," + duration.computed)
    case SequentialSegment(tracks) => {
      print("(")
      for(track <- tracks) prettyPrint(track, ident, scale)
      print(")")
    }
    case ParallelSegment(tracks) => {
      println("(")
      for(track <- tracks) {
        print(ident + defaultIdent)
        prettyPrint(track, ident + defaultIdent, scale)
        println
      }
      print(ident + ")")
    }
  }
}