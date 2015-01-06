package grammarTest

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter
import grammar._
import grammar.GrammarElement._
import rythmics.BPM

class GrammarElementFirstsTest extends FunSuite with Matchers with BeforeAndAfter {

  val e = epsilon[Int]
  val o = Set.empty[Int]

  before { SGIdGen.reset }

  test("epsilon firsts empty") {
     e.firsts shouldBe o
  }

  test("terminal firsts singleton") {
    Word(1).firsts shouldBe Set(1)
  }

  test("empty rule empty firsts") {
    lazy val r = e ** e ** e
    r.firsts shouldBe o
  }

  test("rule terminal after epsilons") {
    lazy val r = e ** e ** 1 ** e
    r.firsts shouldBe Set(1)
  }

  test("empty composed rule") {
    lazy val rule = (e ** e ** e) ** (e ** e)
    rule.firsts shouldBe o
  }

  test("not nullable composed rule") {
    lazy val rule = (1 ** 2 ** 3) ** (4 ** 5)
    rule.firsts shouldBe Set(1)
  }

  test("simple production, no epsilon") {
    lazy val p = (1, 1.0) || (2, 1.0) || (3, 1.0)
    p.firsts shouldBe Set(1, 2, 3)
  }

  test("simple produciton") {
    lazy val p = (1, 1.0) || (3, 1.0) || (e, 2.0) || (3, 1.0)
    p.firsts shouldBe Set(1, 3)
  }

  test("composed production, no epsilon") {
    lazy val p = (1, 1.0) || (2 ** 3 ** 4, 1.0) || (((5 ** 6, 1.0) || (7, 1.0)), 1.0)
    p.firsts shouldBe Set(1, 2, 5, 7)
  }

  test("composed production") {
    lazy val p = (1, 1.0) || (2 ** e ** 4, 1.0) || (((5 ** 6, 1.0) || (e, 1.0)), 7.0)
    p.firsts shouldBe Set(1, 2, 5)
  }

  test("recursive rule") {
    lazy val r: Rule[Int] = e ** 1 ** r
    r.firsts shouldBe Set(1)
  }

  test("soft recursive rule") {
    lazy val r: Rule[Int] = e ** e ** r
    r.firsts shouldBe o
  }

  test("hard recursive rule") {
    lazy val r: Rule[Int] = Rule(List(r))
    r.firsts shouldBe o
  }

  test("firsts of production inside rule") {
    lazy val p = (1, 1.0) || (2, 1.0)
    lazy val r = e ** e ** p ** 3

    r.firsts shouldBe Set(1, 2)
  }

  test("firsts of production after recursive rule") {
    lazy val p1 = (1, 1.0) || (e, 1.0)
    lazy val p2 = (e, 1.0) || (2, 1.0)
    lazy val r: Production[Int] = (p1 ** p2 ** r ** 3, 1.0) || (e, 1.0)

    r.firsts shouldBe Set(1, 2, 3)
  }

  test("empty simple recursive production") {
    lazy val p: Production[Int] = (e, 1.0) || (p, 1.0) 
    p.firsts shouldBe o
  }

  test("non-empty recursive production") {
    lazy val p: Production[Int] = (1, 1.0) || (p, 1.0) || (2 ** 3, 1.0) || (e ** p, 0.0)
    p.firsts shouldBe Set(1, 2)
  }

  test("nullable recursive production") {
    lazy val p: Production[Int] = (1, 1.0) || (p, 1.0) || (2 ** 3, 1.0) || (e ** p, 0.0) || ((e ** 4, 1.0) || (p ** p ** p, 1.0) || (e ** e, 1.0), 1.0)
    p.firsts shouldBe Set(1, 2, 4)
  }


  test("firsts with message") {
    val q = BPM.q
    lazy val ref: RootRythmRefine[Int] = RootRythmRefine(q ** q ** q)
    lazy val g: Rule[Int] = ref ** 1 ** 2

    g.firsts shouldBe Set(1)
  }



}
