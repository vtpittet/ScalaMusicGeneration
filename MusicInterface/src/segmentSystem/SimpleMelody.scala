package segmentSystem

import rythmics.BPM
import tonalSystem.Tone

class SimpleMelody(notes: List[Note]) extends SequentialSegment(notes) {
  def +:(rawNote:(Tone, Int, Int)): SimpleMelody = rawNote match {
    case (tone, bpm, frac) => SimpleMelody(Note(tone, BPM(bpm, frac)) :: notes)
  }
  def +:(rawNote:(Tone, Int)): SimpleMelody = rawNote match {
    case (tone, bpm) => +:(tone, bpm, 0)
  }
}

object SimpleMelody extends SimpleMelody(Nil){
  def apply(notes: List[Note]): SimpleMelody = new SimpleMelody(notes)
  def apply(rawNotes: (Tone, Int, Int)*): SimpleMelody = SimpleMelody(rawNotes.map(_ match {
    case (tone, bpm, frac) => Note(tone, BPM(bpm, frac))
  }).toList)
  
}
