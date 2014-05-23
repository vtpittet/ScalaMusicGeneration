package playingTest

import utils.MelodyWriter
import tonalSystem.I
import segmentSystem.IsNote
import rythmics.E
import segmentSystem.Note
import segmentSystem.MusicalSegment
import midiInterface.MelodyPlayer
import tonalSystem.Major
import tonalSystem.C
import segmentSystem.IsSeq

object ExpandUses extends App with MelodyWriter {
  
  type T = MusicalSegment => MusicalSegment
  val tempo = 60
  val scale = Major(C)
  
  val base = I * 4
  
  
  val t0: T = _ *2 /2
  val t1: T = _ /2 *+ (_ +4)
  val t2: T = _ /2 *+ (_ / 2 *2 +4)
  
//  play(base)
  
  
  val step1 = base ++> (IsNote thenDo (t0, 2))
  
  val step2 = step1 ++> (IsSeq given (_.height == 1) thenDo (t2, 2, 1) or (t1))
  
  play(step2)
  
  def play(m: MS) = MelodyPlayer(m withScale scale, tempo)
}