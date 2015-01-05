package grammarTest

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter
import grammar._
import grammar.SimpleElement._

class SimpleGrammarNullableTest extends FunSuite with Matchers with BeforeAndAfter {

  val e = epsilon[Int]

  before { SGIdGen.reset }

  test("epsilon nullable") {
    epsilon.nullable shouldBe true
  }

  test("terminal not nullable") {
    SimpleWord(1).nullable shouldBe false
  }

  test("nullable rule") {
    lazy val r = e ** e ** e
    r.nullable shouldBe true
  }

  test("not nullable rule") {
    lazy val r = e ** e ** 1 ** e
    r.nullable shouldBe false
  }

  test("nullable composed rule") {
    lazy val rule = (e ** e ** e) ** (e ** e)
    rule.nullable shouldBe true
  }

  test("not nullable composed rule") {
    lazy val rule = (1 ** 2 ** 3) ** (1 ** 2)
    rule.nullable shouldBe false
  }

  test("not nullable production") {
    lazy val p = (1, 1.0) || (2, 1.0) || (3, 1.0)
    p.nullable shouldBe false
  }

  test("nullable produciton") {
    lazy val p = (1, 1.0) || (3, 1.0) || (e, 2.0) || (3, 1.0)
    p.nullable shouldBe true
  }

  test("not nullable composed production") {
    lazy val p = (1, 1.0) || (1 ** 2 ** 3, 1.0) || (((1 ** 2, 1.0) || (0, 1.0)), 1.0)
    p.nullable shouldBe false
  }

  test("nullable composed production") {
    lazy val p = (1, 1.0) || (1 ** 2 ** 3, 1.0) || (((1 ** 2, 1.0) || (e, 1.0)), 1.0)
    p.nullable shouldBe true
  }

  test("not nullable recursive rule") {
    lazy val r: SimpleRule[Int] = e ** 1 ** r
    r.nullable shouldBe false
  }

  test("soft recursive rule") {
    lazy val r: SimpleRule[Int] = e ** e ** r
    r.nullable shouldBe false
  }

  test("hard recursive rule") {
    lazy val r: SimpleRule[Int] = SimpleRule(List(r))
    r.nullable shouldBe false
  }

  test("simple recursive production") {
    lazy val p: SimpleProduction[Int] = (e, 1.0) || (p, 1.0) 
    p.nullable shouldBe true
  }

  test("not nullable recursive production") {
      lazy val p: SimpleProduction[Int] = (1, 1.0) || (p, 1.0) || (1 ** 1, 1.0) || (e ** p, 0.0)
    p.nullable shouldBe false
  }

  test("nullable recursive production") {
    lazy val p: SimpleProduction[Int] = (1, 1.0) || (p, 1.0) || (1 ** 1, 1.0) || (e ** p, 0.0) || ((e ** 1, 1.0) || (p ** p ** p, 1.0) || (e ** e, 1.0), 1.0)
    p.nullable shouldBe true
  }


}
