package gmrowe.projecteuler

import Problem80._
import org.specs._
import org.specs.matcher.Matcher

class Problem80Spec extends Specification {    
   def allSatisfy[A](m: => Matcher[A]): Matcher[Iterable[A]] = 
     new Matcher[Iterable[A]] {
      def apply(is: => Iterable[A]) = is find (e => !(m(e)._1)) match {
        case Some(e) => 
          (false, "", e+" doesn't satisfy requirement: ("+m(e)._3+")")
        case None => (true, "All elements satisfy the expectation", "")
      }
    }
        
    val haveOneDigit = new Matcher[Int] {
      def apply(i: => Int) =
        (i > 0 && i < 10, i+" has 1 digit", i+" has more than 1 digit")
    }
    
    val hasOneDigit = haveOneDigit
    
    val haveTwoDigits = new Matcher[Int] {
      def apply(i: => Int) = 
        (i >= 10 && i < 100, i+" has 2 digits", i+" does not have 2 digits")
    }
    
    val hasTwoDigits = haveTwoDigits
    
    "Problem80.groupByTwos" should {
      "Group a single digit number into a singleton list" in {
        groupByTwos(1) mustEqual List(1)
      }
      "Group a two digit number into a singleton list" in {
        groupByTwos(12) mustEqual List(12)
      }
      "Group a three digit number into a List with one digit first element " +
      "and a two digit second element" in {
        groupByTwos(123) mustEqual List(1, 23)
      }
      "Have two digit groupings for any element other than the first" in {
        val beEmptyOrBeAllTwoDigits = beEmpty or hasTwoDigits.toIterable
        val testCases = List(1, 12, 123, 1234, 12345, 123456, 1234567)
        testCases map (groupByTwos(_).tail) map (_ must beEmptyOrBeAllTwoDigits)
      }
      "Have a single digit first element for when the argument has an odd " +
      "number of digits" in {
        val testCases = List(1, 123, 12345, 1234567)
        testCases map (groupByTwos(_).head) must haveOneDigit.toIterable
      }
      "Be composable to the starting number" in {
        def compose(ns: List[Int]): Int = ns map (_.toString) reduceLeft (_ + _) toInt
        
        val testValues = List(1111, 1, 23155, 132453, 1234321)
        testValues map { n =>
          val grouped = groupByTwos(n)
          compose(grouped) must beEqualTo(n)
        }
      }
    }

    "sqrtEstimate(n)" should {
      "result in an integer i for which i * i <= n" in {
       val testCases = List(0, 1, 2, 4, 100, 1000, 10000, 546923)
       testCases map { n => 
         val sqrtEst = sqrtEstimate(n)
         (sqrtEst * sqrtEst) must beLessThanOrEqualTo(n)
       }
      }
      "result in the largest integer i for which i * i <= n" in {
       val testCases = List(0, 1, 2, 4, 100, 1000, 10000, 546923)
       testCases map { n =>
         val sqrtEst = sqrtEstimate(n)
         val succ = sqrtEst + 1
         (succ * succ) must beGreaterThan(n)
       }
      }
    }
   
  "nextDigit" should {
     "return a tuple in which first element is the largest digit \'n\' that " +
     " satisfies the relation ((partialSolution * 20) + n) * n <= target" in {
       nextDigit(12, 827)._1 mustEqual 3
     }
  }
   
  "composeToDecimal" should {
    "compose a tuple with an empty fractional list" in {
      composeToDecimal(List(1) -> Nil) must beEqualTo(1.0)
    }
    "compose a tuple with an empty whole number list" in {
      composeToDecimal(Nil -> List(1)) must beEqualTo(0.1)
    }
    "compose a tuple with non-empty whole number and fractional List" in {
      composeToDecimal(List(1) -> List(1)) must beEqualTo(1.1)
      composeToDecimal(List(1,2,3) -> List(4,5,6)) must beEqualTo(123.456)
    }
  }
  
  // "sqrt" should {
    // "give an estimate of a square root to the designated number of " +
    // "digits" in {
      // val _1e_100 = BigDecimal("0." + ("0" * 100) + "1", java.math.MathContext.UNLIMITED)
      // sqrt(1)(_1e_100) mustEqual 1.0
      // sqrt(4)(_1e_100) mustEqual 2.0
      // sqrt(2)(_1e_100) mustEqual 1.3
    // }
  // }
}