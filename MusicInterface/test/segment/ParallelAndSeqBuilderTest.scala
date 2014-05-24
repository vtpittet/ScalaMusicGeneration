package segment

import org.scalatest.FunSuite
import segmentSystem.SequentialSegment
import segmentSystem.MusicalSegment
import utils.MelodyWriter
import segmentSystem.ClassPredicate
import tonalSystem.Tone._
import segmentSystem.IsSeq

class ParallelAndSeqBuilderTest extends FunSuite with MelodyWriter {


  implicit val sequentialBuilder: List[MS] => SS = ALevel(_)
  val melody = I ++ I ++ I ++ I ++ I
  //             A    B    C    Seq
  
  test("ALevel selection") {
    val after = melody ++> (IsA thenDo (_ + 1))
    val expected = II ++ II ++ I ++ I ++ I
    assert(after == expected)
  }
  
  test("BLevel selection") {
    val after = melody ++> (IsB thenDo (_ + 2))
    val expected = III ++ III ++ III ++ I ++ I
    assert(after == expected)
  }
  
  test("CLevel selection") {
    val after = melody ++> (IsC thenDo (_ + 3))
    val expected = IV ++ IV ++ IV ++ IV ++ I
    assert(after == expected)
  }
  
  test("Seq default top level selection") {
    val after = melody ++> (IsSeq thenDo (_ + 4))
    val expected = V ++ V ++ V ++ V ++ V
    assert(after == expected)
  }
}


case class ALevel(m: List[MusicalSegment]) extends SequentialSegment(m) {
  val buildFromMelody = ALevel(_)
  override val sequentialBuilder = BLevel(_)
}

case class BLevel(m: List[MusicalSegment]) extends SequentialSegment(m) {
  val buildFromMelody = BLevel(_)
  override val sequentialBuilder = CLevel(_)
}

case class CLevel(m: List[MusicalSegment]) extends SequentialSegment(m) {
  val buildFromMelody = CLevel(_)
}

object IsA extends ClassPredicate[ALevel](_ match {case n: ALevel => n})
object IsB extends ClassPredicate[BLevel](_ match {case n: BLevel => n})
object IsC extends ClassPredicate[CLevel](_ match {case n: CLevel => n})