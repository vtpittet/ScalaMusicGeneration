package generation

import grammar._
import chord.Chord
import rythmics.BPM
import rythmics.RythmCell
import tonalSystem.Tone
import segmentSystem._
import midiInterface.MelodyPlayer


class Generator(
  harm: GrammarElement[Chord],
  rootRythm: GrammarElement[BPM],
  rythm: GrammarElement[RythmCell],
  melody: GrammarElement[Tone],
  closeWithH: PartialSolution[_] => Boolean
) {

  def generateMusic = generate(0)

  def generateMusicWithChords = generateWithChords(0)

  def generate(minLength: Int): MusicalSegment = {
    Generator.solToSegment(genOnly(minLength))
  }

  def generateWithChords(minLength: Int) : MusicalSegment = {
    Generator.solToSegmentWithChords(genOnly(minLength))
  }

  def genOnly(minLength: Int): Harm = {
    val initSols: List[Harm] = List(Harm(
      ParsingTree(harm),
      ParsingTree(rootRythm),
      ParsingTree(rythm),
      ParsingTree(melody),
      closeWithH
    ))

    val closCond: Harm => Boolean = { ps: Harm =>
      ps.h.wordSize >= minLength && ps.completed
    }

    Generator.electOne(Generator.rGenerate(initSols, closCond)).get
  }

}

object Generator {
  def apply(
    h: GrammarElement[Chord],
    rr: GrammarElement[BPM],
    rc: GrammarElement[RythmCell],
    m: GrammarElement[Tone],
    rrCWH: Boolean = false,
    rcCWH: Boolean = false,
    mCWH: Boolean = false
  ): Generator = {
    def closeWithH(ps: PartialSolution[_]): Boolean = ps match {
      case h: Harm => true
      case rr: Root => rrCWH
      case rc: Cell => rcCWH
      case m: Melody => mCWH
    }

    new Generator(h, rr, rc, m, closeWithH)
  }

  val globalProb: PartialSolution[_] => Double = _.prob

  def elect[A <: PartialSolution[A]](target: List[A]) = ParsingTree.elect(target)(globalProb)

  def normalize[A <: PartialSolution[A]](target: List[A]): List[A] =
    PartialSolution.normalize(target)

  def electOne[A <: PartialSolution[A]](target: List[A]): Option[A] = {
    ParsingTree.electOne(target)(globalProb)._1
  }

  // returns at the first solution found
  // if one call of rGenerate miss, return one previous step
  def rGenerate(sols: List[Harm], closCond: Harm => Boolean): List[Harm] = {
    //println("iteration in generator")
    val gen: Harm => List[Harm] = oneStepGen(_)
    
    //println("call oneStepGen with sols.size = " + sols.size)

    sols flatMap gen match {
      case Nil => {
        println("[warn]: no solution found, returning longest partial solution")
        Generator.normalize(Generator.elect(sols)) // failure
      }
      case newSols if newSols exists closCond =>
        Generator.normalize(Generator.elect(newSols filter closCond))
      case newSols => {
        rGenerate(Generator.normalize(Generator.elect(newSols)), closCond)
      }
    }
  }

  /** generates one big step, that is one step for each ParsingTree
    */
  def oneStepGen(start: Harm): List[Harm] = {

    def elect[A <: PartialSolution[A]](target: List[A]) = Generator.elect(target)

    def normalize[A <: PartialSolution[A]](target: List[A]) =
      PartialSolution.normalize(target)

    //println("gen chords")
    val stepRoot: List[Root] = normalize(elect(start.gen))
    //println("stepRoot : " + stepRoot.size)

    //println("gen root")
    val stepCell: List[Cell] = normalize(elect(stepRoot flatMap (_.gen)))
    //println("stepCell : " + stepCell.size)

    //println("gen cells")
    val stepMelody: List[Melody] = normalize(elect(stepCell flatMap (_.gen)))
    //println("stepMelody : " + stepMelody.size)

    stepMelody match {
      case Nil => Unit
      case hd :: tl => if (hd.m.wordSize == 4 && hd.m.stack.size == 1) {
        println("gotcha")
        println()
      }
    }
    //println("gen melody")
    val stepHarm: List[Harm] = normalize(elect(stepMelody flatMap (_.gen)))
    //println("stepHarm : " + stepHarm.size)


    stepHarm
  }

  def solToSegment(sol: Harm): Sequential = {
    val Harm(h, rr, rc, m, _) = sol
    val roots = rr.collectWords
    val cells = rc.collectWords
    val tones = m.collectWords

    val durations: List[BPM] = roots zip cells flatMap {
      case (rr, rc) => rc.durations map { _ / (rc.duration / rr.computed) }
    }
    val notes = tones zip durations map {
      case (tone, duration) => Note(tone, duration)
    }

    Sequential(notes)
  }

  def solToChords(sol: Harm): MusicalSegment = {
    val Harm(h, rr, rc, m, _) = sol
    val chords = h.collectWords
    val roots = rr.collectWords
    
    val harms = chords zip roots map {
      case (chord, bpm) => Parallel(chord.getTones map (Note(_, bpm)))
    }

    Sequential(harms)
  }

  def solToSegmentWithChords(sol: Harm): Parallel = {
    Parallel(List(solToSegment(sol), solToChords(sol)))
  }

}
