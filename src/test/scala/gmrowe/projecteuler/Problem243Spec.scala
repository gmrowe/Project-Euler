package gmrowe.projecteuler

import org.specs._
import Problem243._
import EulerUtils._

class Problem243Spec extends Specification {
  "admissable" should {
    "return true for known qualifiers" in {
      val qualifiers = List(2,4,6,8,12,16,18,24,30,32,36,48)
      qualifiers map (q => admissable(q) must beTrue)
    }
  }
  
  "isPowerOf2" should {
    "return true if a number is a power of 2" in {
      val powsOf2 = List(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
      powsOf2 map (ps => isPowerOf2(ps) must beTrue)
    }
    "return false if a number is not a power of 2" in {
      val nonPowsOf2 = List(3, 5, 9, 17, 33, 65, 129, 257, 513, 1025)
      nonPowsOf2 map (nps => isPowerOf2(nps) must beFalse)
    }
  }
  
  "are consecutive primes" should {
    "return true if the argument List has elements which are all prime, is " +
    "sorted, and no prime number exists between two list elements" in {
      areConsecutivePrimes(List(2)) must beTrue
      areConsecutivePrimes(List(2, 3)) must beTrue
      areConsecutivePrimes(List(3, 5, 7)) must beTrue
    }
    "return false if the argument list is not sorted, or there is an " +
    "intevening prime missing, or one of the elements is not prime" in {
      areConsecutivePrimes(List(3, 2)) must beFalse
      areConsecutivePrimes(List(2, 5)) must beFalse
      areConsecutivePrimes(List(3, 5, 6)) must beFalse
    }
  }
}