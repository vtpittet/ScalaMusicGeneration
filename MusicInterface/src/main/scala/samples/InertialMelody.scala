package samples

import tonalSystem.Tone
import tonalSystem.Tone.I
import grammar.ImplicitsWords2Elements._
import grammar.Epsilon

/** Calls to generation algorithm and details of melody producing
  * plus some helping function are implemented in Presentation trait
  * Extending element only has to define the four grammars :
  * chords, root, cells and tones
  * and call play() to compose and play the music.
  */
trait InertialMelodySpec extends PurelyRandomSpec {
  
  // tuning parameters
  val pertFactor: Double = 0.0
  val zeroAttraction: Double = 0.5
  val zeroDeviation: Int = 5
  val initialInertia: Int = 8

  // helping function implementing "inertial" melody
  def inertialNext(t: Tone): Grammar[Tone] = {
    // positive if t2 higher, negative if t2 lower
    def d0(t2: Tone): Int = (t stepsTo t2) - zeroDeviation
    def perturbation : Int = (scala.util.Random.nextGaussian() * pertFactor).toInt
    def rec(t: Tone, inertia: Int, deviation: Int): Grammar[Tone] = {
      val zeroTend = (- d0(t) * zeroAttraction).toInt
      val newInertia = inertia - deviation + perturbation + zeroTend
      val factorDown = if (newInertia > 0) 0.2 else 1.0
      val factorUp = if (newInertia < 0) 0.2 else 1.0
      val newT = t increaseBy deviation
      (newT) ** (
        (rec(newT, newInertia, -2), 1.0 * factorDown) ||
        (rec(newT, newInertia, -1), 2.0 * factorDown) ||
        (rec(newT, newInertia, 0), 0.5)               ||
        (rec(newT, newInertia, 1), 2.0 * factorUp)    ||
        (rec(newT, newInertia, 2), 1.0 * factorUp)    ||
        (Epsilon[Tone](), 1.0) // making it endable, useful later
      )
    }
    rec(t, initialInertia, 0)
  }

  override
  lazy val tones = inertialNext(I)

}

object InertialMelody extends App with InertialMelodySpec { play }
