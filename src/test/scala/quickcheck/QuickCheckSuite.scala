package quickcheck

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{const, oneOf}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalacheck.{Gen, Prop}
import org.scalatest.exceptions.TestFailedException

object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap

@RunWith(classOf[JUnitRunner])
class QuickCheckSuite extends FunSuite with Checkers {
  def checkBogus(p: Prop) {
    var ok = false
    try {
      check(p)
    } catch {
      case e: TestFailedException =>
        ok = true
    }
    assert(ok, "A bogus heap should NOT satisfy all properties. Try to find the bug!")
  }

  test("Binomial heap satisfies properties.") {
    check(new QuickCheckHeap with quickcheck.test.BinomialHeap)
  }

  test("Bogus (1) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus1BinomialHeap)
  }

  test("Bogus (2) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus2BinomialHeap)
  }

  test("Bogus (3) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus3BinomialHeap)
  }

  test("Bogus (4) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus4BinomialHeap)
  }

  test("Bogus (5) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus5BinomialHeap)
  }
}
