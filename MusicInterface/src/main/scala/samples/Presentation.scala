package samples

import grammar.Epsilon
import grammar.ImplicitsWords2Elements._
import generation.Generator
import chord.Chord
import rythmics.BPM
import rythmics.RythmCell
import tonalSystem.Tone
import midiInterface.MelodyPlayer
import segmentSystem.MusicalSegment


/** Trait Presentation abstracts namings and
  * call mechanisms. The extendings objects define
  * chords, root, cells and tones grammars and then
  * call play() to run the melody
  */
trait Presentation {
  type RootRythm = BPM


  val chords: Grammar[Chord]
  val root: Grammar[RootRythm]
  val cells: Grammar[RythmCell]
  val tones: Grammar[Tone]

  var closeRoot: Boolean = false
  var closeCells: Boolean = false
  var closeTones: Boolean = false

  // repeats the same grammar g n times
  def repeat[T](n: Int)(g: Grammar[T]): Grammar[T] =
    if (n > 0) g ** repeat(n-1)(g)
    else Epsilon[T]()

  // used to switch to generateMusicWithChords
  val useGenerator: Generator => MusicalSegment = _.generateMusic


  def play: Unit = {
    val music = useGenerator(Generator(
      chords, root, cells, tones,
      closeRoot, closeCells, closeTones))

    val tempo: Int = 80

    MelodyPlayer(music, tempo) 
  }

}
