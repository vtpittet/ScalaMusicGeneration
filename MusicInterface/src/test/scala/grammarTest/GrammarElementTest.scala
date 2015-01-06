package grammarTest

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter
import grammar._
import grammar.GrammarElement._
import rythmics._

class GrammarElementTest extends FunSuite with Matchers with BeforeAndAfter {

  before { GEIdGen.reset }

  test("Implicit simple word") {
    val word: GrammarElement[Int] = 1
    word shouldBe Word(1)
  }

  test("Rule no recursion") {
    val rule: Rule[Int] = 1 ** 2 ** 3
    rule compareBody Rule(1, 2, 3) shouldBe true
  }

  test("implicit simple production") {
    val prod: Production[Int] = (1, 2.0)
    prod compareBody Production((1, 2.0)) shouldBe true
  }

  test("Production no recursion") {
    val prod: Production[Int] = (1, 2.0) || (2, 1.0) || (3, 0.0)
    prod compareBody Production((1, 2.0), (2, 1.0), (3, 0.0)) shouldBe true
  }

  test("Complex expression no recursion") {
    val generated: Rule[Int] = (
      (1 ** 2 ** 4, 2.0) ||
      (4 ** 2 ** 1, 1.0) ||
      (1, 1.0)
    ) ** 5

    val created = Rule(List(
      Production(List(
        (Rule(1, 2, 4), 2.0),
        (Rule(4, 2, 1), 1.0),
        (Word(1), 1.0)
      )),
      Word(5)
    ))

    generated compareBody created shouldBe true
  }

  test("recursive rule") {
    lazy val r: Rule[Int] = 0 ** 1 ** r
    val rString = "R_1:(0 ** 1 ** R_1)"

    r.toName shouldBe r.body(2).toName
    r.toString shouldBe rString
  }

  test("recursive production") {
    lazy val p: Production[Int] = (0, 2.0) || (p, 1.0)
    val pString = "P_1:((0, 2.0) || (P_1, 1.0))"

    p.toName shouldBe p.body(1)._1.toName
    p.toString shouldBe pString
  }

  test("recursion first in production") {
    /* following declaration does not work, as p refers to itself
     *  it is not wrapped inside a Production element :

     *  lazy val p: Production[Int] = (p, 1.0) || (1, 1.0)
     */

    lazy val p: Production[Int] =
      Production(List((p, 1.0))) || (1, 1.0)
    val pString = "P_1:((P_1, 1.0) || (1, 1.0))"

    p.toName shouldBe p.body(0)._1.toName
    p.toString shouldBe pString
  }

  test("Mutually recursive rules") {
    lazy val r1: Rule[Int] = 1 ** r2
    lazy val r2: Rule[Int] = 2 ** r1

    val r1String = "R_1:(1 ** R_2:(2 ** R_1))"
    r1.toName//side effect
    r1.toName shouldBe "R_1"
    r2.toName shouldBe r1.body(1).toName
    r1.toString shouldBe r1String
  }

  test("To string bug regression") {
    lazy val r1: Rule[Int] = 1 ** r2
    lazy val r2: Rule[Int] = 2 ** r2

    val r1String = "R_1:(1 ** R_2:(2 ** R_2))"
    r1.toName//side effect
    r1.toName shouldBe "R_1"
    
    r1.toString shouldBe r1String
  }

  test("Grammar with message") {
    val q = BPM.q
    lazy val rref: Rule[BPM] = q ** q ** q
    lazy val ref: RythmRefine[Int] = RythmRefine(rref)
    lazy val g: Rule[Int] = ref ** 1 ** 2 ** ref

    lazy val gen: Rule[Int] = new Rule(List(ref, new Word(1), new Word(2), ref))

    gen compareBody g shouldBe true
  }
}
