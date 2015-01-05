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

}
