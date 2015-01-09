package ruledGen

import grammar._
import grammar.ImplicitsWords2Elements._
import generation._
import chord._
import rythmics._
import rythmics.RythmCell
import rythmics.RythmCell._
import tonalSystem.Tone.{I, II, III, IV, V, VI, VII}
import tonalSystem.Tone
import midiInterface.MelodyPlayer


object Base extends App {


  lazy val chords: Grammar[Chord] = 
    Triad(I) ** Triad(IV) ** Triad(V) ** Triad(I)

  lazy val root: Grammar[BPM] = H ** Q ** root

  lazy val cells: Grammar[RythmCell] = (Q +: E +: E) ** cells

  lazy val tones: Grammar[Tone] = gen(I)


  def gen(t: Tone): Grammar[Tone] = t ** (
    (gen(t decreaseBy 2), 2.0) ||
    (gen(t decreaseBy 1), 4.0) ||
    (gen(t), 1.0) ||
    (gen(t increaseBy 1), 4.0) ||
    (gen(t increaseBy 2), 2.0)
  )

  Generator(chords, root, cells, tones).generate(0) match {
    case None => println("nothing returned, first gen should have failed")
    case Some(music) => MelodyPlayer(music, 60)
  }

  

}
