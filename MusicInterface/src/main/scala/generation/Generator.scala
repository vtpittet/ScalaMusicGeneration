package generation

import grammar._
import chord.Chord
import rythmics.BPM
import rythmics.RythmCell
import tonalSystem.Tone
import segmentSystem.Sequential
import segmentSystem.Note
import midiInterface.MelodyPlayer


class Generator(
  harm: GrammarElement[Chord],
  rootRythm: GrammarElement[BPM],
  rythm: GrammarElement[RythmCell],
  melody: GrammarElement[Tone]
) {


  def solToSegment(sol: Harm): Sequential = {
    val Harm(h, rr, rc, m) = sol
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

  def generate(minLength: Int = 0): Option[Sequential] = {
    val initSols: List[Harm] = List(Harm(
      ParsingTree(harm),
      ParsingTree(rootRythm),
      ParsingTree(rythm),
      ParsingTree(melody)
    ))

    Generator.electOne(rGenerate(initSols, minLength)) map (solToSegment(_))
  }

  // returns at the first solution found
  // if one call of rGenerate miss, return one previous step
  def rGenerate(sols: List[Harm], minLength: Int): List[Harm] = {
    val gen: Harm => List[Harm] = oneStepGen(_)
    
    sols flatMap gen match {
      case Nil => Generator.normalize(Generator.elect(sols)) // failure
      case newSols if minLength <= 0 && newSols.exists(_.completed) =>
        Generator.normalize(Generator.elect(newSols filter (_.completed)))
      case newSols => {
        val mL = if (minLength <= 0) 0 else minLength - 1
        rGenerate(Generator.normalize(Generator.elect(newSols)), mL)
      }
    }
  }

  /** generates one big step, that is one step for each ParsingTree
    */
  def oneStepGen(start: Harm): List[Harm] = {

    def elect[A <: PartialSolution[A]](target: List[A]) = Generator.elect(target)

    def normalize[A <: PartialSolution[A]](target: List[A]) =
      PartialSolution.normalize(target)

    val stepRoot: List[Root] = normalize(elect(start.gen))

    val stepCell: List[Cell] = normalize(elect(stepRoot flatMap (_.gen)))

    val stepMelody: List[Melody] = normalize(elect(stepCell flatMap (_.gen)))

    val stepHarm: List[Harm] = normalize(elect(stepMelody flatMap (_.gen)))

    stepHarm
  }

}

object Generator {
  def apply(
    h: GrammarElement[Chord],
    rr: GrammarElement[BPM],
    r: GrammarElement[RythmCell],
    m: GrammarElement[Tone]
  ): Generator = new Generator(h, rr, r, m)

  val globalProb: PartialSolution[_] => Double = _.prob

  def elect[A <: PartialSolution[A]](target: List[A]) = ParsingTree.elect(target)(globalProb)

  def normalize[A <: PartialSolution[A]](target: List[A]): List[A] =
    PartialSolution.normalize(target)

  def electOne[A <: PartialSolution[A]](target: List[A]): Option[A] = {
    ParsingTree.electOne(target)(globalProb)._1
  }

}
