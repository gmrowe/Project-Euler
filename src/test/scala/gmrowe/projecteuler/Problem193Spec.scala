package gmrowe.projecteuler

import Problem193._
import org.specs._
import EulerUtils._

class Problem193Spec extends Specification {
  "longPow(x, y)" should {
    "return x^y as a long" in {
      longPow(2L, 0L) mustEqual 1L
      longPow(2L, 1L) mustEqual 2L
      longPow(2L, 4L) mustEqual 16L
    }
    "throw an exception for a negative exponent" in {
      longPow(2L, -1L) must throwAn[Exception]
    }
  }
  
  "isSquarefree(n)" should {
    "return true for known squareFreeNumbers" in {
      List(1L, 2L, 3L, 5L, 6L, 7L, 10L, 11L) map { n =>
        isSquarefree(n) must beTrue
      }
    }
  }
}