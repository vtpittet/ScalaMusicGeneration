package grammarTest

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter
import grammar._
import grammar.SimpleElement._

class SimpleGrammarTest extends FunSuite with Matchers with BeforeAndAfter {

  before { SGIdGen.reset }

  test("Implicit simple word") {
    val word: SimpleElement[Int] = 1
    word shouldBe SimpleWord(1)
  }

  test("Rule no recursion") {
    val rule: SimpleRule[Int] = 1 ** 2 ** 3
    rule.body shouldBe SimpleRule(1, 2, 3).body
  }

  test("implicit simple production") {
    val prod: SimpleProduction[Int] = (1, 2.0)
    prod.body shouldBe SimpleProduction((1, 2.0)).body
  }

  test("Production no recursion") {
    val prod: SimpleProduction[Int] = (1, 2.0) || (2, 1.0) || (3, 0.0)
    prod.body shouldBe SimpleProduction((1, 2.0), (2, 1.0), (3, 0.0)).body
  }

  test("bug research") {
    val gen: SimpleRule[Int] = 1 ** 2 ** 3
    println(gen)
  }

  test("Complex expression no recursion") {
    val generated: SimpleRule[Int] = (
      (1 ** 2 ** 4, 2.0) ||
      (4 ** 2 ** 1, 1.0) ||
      (1, 1.0)
    ) ** 5

    val created = SimpleRule(List(
      SimpleProduction(List(
        (SimpleRule(1, 2, 4), 2.0),
        (SimpleRule(4, 2, 1), 1.0),
        (SimpleWord(1), 1.0)
      )),
      SimpleWord(5)
    ))

    generated compareBody created shouldBe true
  }

  test("recursive rule") {
    lazy val r: SimpleRule[Int] = 0 ** 1 ** r
    val rString = "R_1:(0 ** 1 ** R_1)"

    r.toName shouldBe r.body(2).toName
    r.toString shouldBe rString
  }

  test("recursive production") {
    lazy val p: SimpleProduction[Int] = (0, 2.0) || (p, 1.0)
    val pString = "P_1:((0, 2.0) || (P_1, 1.0))"

    p.toName shouldBe p.body(1)._1.toName
    p.toString shouldBe pString
  }

  test("recursion first in production") {
    /* following declaration does not work, as p refers to itself
     *  it is not wrapped inside a SimpleProduction element :

     *  lazy val p: SimpleProduction[Int] = (p, 1.0) || (1, 1.0)
     */

    lazy val p: SimpleProduction[Int] =
      SimpleProduction(List((p, 1.0))) || (1, 1.0)
    val pString = "P_1:((P_1, 1.0) || (1, 1.0))"

    p.toName shouldBe p.body(0)._1.toName
    p.toString shouldBe pString
  }

  test("Mutually recursive rules") {
    lazy val r1: SimpleRule[Int] = 1 ** r2
    lazy val r2: SimpleRule[Int] = 2 ** r1

    val r1String = "R_1:(1 ** R_2:(2 ** R_1))"
    r1.toName//side effect
    r1.toName shouldBe "R_1"
    r2.toName shouldBe r1.body(1).toName
    r1.toString shouldBe r1String
  }

  test("To string bug regression") {
    lazy val r1: SimpleRule[Int] = 1 ** r2
    lazy val r2: SimpleRule[Int] = 2 ** r2

    val r1String = "R_1:(1 ** R_2:(2 ** R_2))"
    r1.toName//side effect
    r1.toName shouldBe "R_1"
    
    r1.toString shouldBe r1String
  }

}
