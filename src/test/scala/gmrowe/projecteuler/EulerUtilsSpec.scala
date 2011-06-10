package gmrowe.projecteuler

import org.specs._
import EulerUtils._
import scala.math._

class EulerUtilsSpec extends Specification {
   
   "EulerUtils.factorize" should {
      "handle 1" in {
         factorize(1) mustEqual List((1, 1))
      }
      
      "throw IllegalArgumentException on non-positive inputs" in {
         factorize(-1) must throwAn[IllegalArgumentException]
      }
      
      "factor prime numbers to just 1 and the number" in {
         factorize(7) mustEqual List((1, 7))
      }
      
      "factor composite numbers to all possible representations" in {
         factorize(10) mustEqual List((1, 10), (2, 5))
      }
      
      "factor a number I'm interested in" in {
         factorize(7254) must contain((39, 186))
      }
      
      "handle perfect squares gracefully" in {
         factorize(9) mustEqual List((1, 9), (3, 3))
      }
   }
   
   "EulerUtils.decompose" should {
      "throw IllegalArgumentException on negative input" in {
         decompose(-1) must throwAn[IllegalArgumentException]
      }
      
      "decompose a single digit number into a singleton list" in {
         val n = 1
         decompose(n) mustEqual List(n)
      }
      
      "decompose a multi-digit number into a list containing the digits" in {
         (decompose(0) mustEqual List(0))  && 
            (decompose(100) mustEqual List(1, 0, 0)) &&
            (decompose(45678) mustEqual List(4, 5, 6, 7, 8))
      }
   }
   
   "EulerUtils.gcd" should {
      "return the largest int that is evenly divisible by both arguments" in {
         val lim = 10000
         def makeRandomInt = (random * lim).toInt
         
         val x = makeRandomInt
         val y = makeRandomInt
         
         val d = gcd(x, y)
         
         def isXYFactor(z: Int) = x % z == 0 && y % z == 0
         val factsGreaterThanD = ((d + 1) to x.max(y)).filter (isXYFactor)
         
         ((x % d) mustEqual 0) &&
            ((y % d) mustEqual 0) &&
            (factsGreaterThanD.size mustEqual 0)
            
      }
   }
   
   "EulerUtils.compose" should {
      "return zero for an empty list" in {
         compose(List()) mustEqual 0
      }
      
      "return a single digit number for a singleton list" in {
         val n = 1
         compose(List(n)) mustEqual n
      }
      
      "be complimentary to decompose" in {
         val n = 144
         compose(decompose(n)) mustEqual n
      }
   }
   
   "EulerUtils.rotateLeft" should {
      "leave a single digit number unchanged" in {
         val n = 9
         rotateLeft(n) mustEqual n
      }
      
      "leave a number with all the same digits unchanged" in {
         val n = 11
         rotateLeft(n) mustEqual n
      }
      
      "Swap positions in a two-digit number" in {
         val n = 42
         rotateLeft(n) mustEqual 24
      }
      
      "rotate a mulit-digit number leftward" in {
         val n = 123
         (rotateLeft(n) mustEqual 231) && (rotateLeft(1234) mustEqual 2341)
      }
   }
   
   "EulerUtils.log10" should {
      "compute correct results for powers of 10" in {
         val tests = List((1, 0),
                          (10, 1),
                          (100, 2),
                          (1000, 3),
                          (10000, 4),
                          (100000, 5),
                          (1000000, 6))
         val allPass = tests.foldRight (true) ((pr, b) => b && (EulerUtils.log10(pr._1).toInt == pr._2))
         allPass must beTrue
      }
      
      "compute correct results for powers of 10 - 1" in {
         val tests = List((9, 0),
                          (99, 1),
                          (999, 2),
                          (9999, 3),
                          (99999, 4),
                          (999999, 5),
                          (9999999, 6))
         val allPass = tests.foldRight (true) ((pr, b) => b && (EulerUtils.log10(pr._1).toInt == pr._2))
         allPass must beTrue
      }
   }
   
   "EulerUtils.primesBelow" should {
      "produce a sorted list of all prime numbers less than the arg value" in {
         primesBelow(40) mustEqual List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
      }
   }
   
   "EulerUtils.isPalindrome" should {
      "return true for an empty list" in {
         isPalindrome(List()) must beTrue
      }
      "return true for a singleton List" in {
         isPalindrome(List(9)) must beTrue
      }
      "return true for a palindromic List" in {
         isPalindrome(List(1, 9, 1)) must beTrue
      }
      "return false for an non=palindromic List" in {
         isPalindrome(List(4, 2)) must beFalse
      }
   }   
   
   "EulerUtils.isPandigital" should {
      "return true for sorted list containing 1 - 9" in {
         isPandigital(123456789) must beTrue
      }
      "return true for an unsorted list containing 1 - 9" in {
         isPandigital(246813579) must beTrue
      }
      "return false if any number 1 - 9 is omitted" in {
         isPandigital(112345678) must beFalse
      }
      "return false for any List with more than 8 entries" in {
         isPandigital(12345678) must beFalse
      }
   }
   
   "EulerUtils.fac" should  {
        
      "correctly compute the factorial of the argument" in { 
         "compute results that agree with literature" in {
           "correctly compute fac(0)" in { fac(0) mustEqual 1 }
	   "correctly compute fac(10)" in { fac(10) mustEqual 3628800 }
           "correctly compute fac(12)" in { fac(12) mustEqual 479001600 }
         }
      }
      
      "throw an IllegalArgumentException when provided a negative argument" in {
         fac(-1) must throwAn[IllegalArgumentException]
      }

   }
   
   "EulerUtils.permutations" should {
      "return a list containing all unique permutations of the input." in {
         val input = "abc"
         permutations(input).size mustEqual fac(input.size)
      }
      
      "provide unique permutations if no elements in the sequence are repeated" in {
         val input = "abc"
         val perms = permutations(input).map (chars => new String(chars.toArray))
         perms.size mustEqual (Set() ++ perms).size
      }
   }
   
   "EulerUtils.primeFactors" should {
     "return a list with all prime factors" in {
        val num = 100
        val facs = primeFactors(num)
        facs.forall(isPrime(_)) must beTrue
        facs.forall(num % _ == 0) must beTrue
     }
     "return a list who's product is the original number" in {
        val num = 100
        val facs = primeFactors(num)
        (facs.foldLeft (1) (_ * _)) mustEqual num
     }
     "return a singleton list when the argument is prime" in {
        val prime = 13
        val fac = primeFactors(prime)
        fac.size mustEqual 1
        fac.head mustEqual prime
     }
   }
   
   "EulerUtils.isPerfectSquare" should {
     "Return true for all perfect squares" in {
       val squares = (1L to 10000L) map (n => n * n)
       squares map (s => isPerfectSquare(s) must beTrue)
     }
     "Return false for non squares" in {
       val nonSquares = (3L to 10000L) map (n => (n * n) - 1)
       nonSquares map (ns => isPerfectSquare(ns) must beFalse)
     }
   }
        
}