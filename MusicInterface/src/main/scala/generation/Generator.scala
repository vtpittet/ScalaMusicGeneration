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
  melody: GrammarElement[Tone],
  closingCondition: PartialSolution[_] => Boolean
) {

  def generateMusic = generate(0)

  def generate(minLength: Int): Sequential = {
    Generator.solToSegment(Generator.electOne(genOnly(minLength)).get)
  }

  def genOnly(minLength: Int): List[Harm] = {
    val initSols: List[Harm] = List(Harm(
      ParsingTree(harm),
      ParsingTree(rootRythm),
      ParsingTree(rythm),
      ParsingTree(melody)
    ))

    val closCond: Harm => Boolean = { ps: Harm =>
      ps.h.wordSize >= minLength && closingCondition(ps)
    }

    Generator.rGenerate(initSols, closCond)
  }

}

object Generator {
  def apply(
    h: GrammarElement[Chord],
    rr: GrammarElement[BPM],
    r: GrammarElement[RythmCell],
    m: GrammarElement[Tone],
    cCond: PartialSolution[_] => Boolean = _.completed
  ): Generator = new Generator(h, rr, r, m, cCond)

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
    println("iteration in generator")
    val gen: Harm => List[Harm] = oneStepGen(_)
    
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

    val stepRoot: List[Root] = normalize(elect(start.gen))

    val stepCell: List[Cell] = normalize(elect(stepRoot flatMap (_.gen)))

    val stepMelody: List[Melody] = normalize(elect(stepCell flatMap (_.gen)))

    val stepHarm: List[Harm] = normalize(elect(stepMelody flatMap (_.gen)))

    println(stepRoot.size)
    println(stepCell.size)
    println(stepMelody.size)
    println(stepHarm.size)

    stepHarm
  }

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

}
