package gmrowe.projecteuler

import org.specs._
import org.specs.matcher.Matcher
import scala.math._

class ProjectEulerSpec extends Specification("Project Euler specification") {
  "Euler7.primes(0)" should {
    "have 0 elements" in { Euler7.primes(0) must beEmpty }
  }
  
  "Euler7.primes(100)" should {
    val size = 100
    val primes = Euler7.primes(size)
    "contain 100 elements" in { primes must haveSize(size)}
    "contain all unique elements" in { primes.size mustEqual Set(primes: _*).size }
    "contain all prime elements" in { primes.forall (_ must bePrime) }
  }
  
  def bePrime = new Matcher[Int] {
    def apply(num: => Int) = {
      def isOdd(n: Int) = (n & 1) == 1
      def isPrime(n: Int) = n == 2 || isOdd(n) && (3 to sqrt(n).toInt by 2).forall (n % _ != 0)
      (isPrime(num), num+" is prime", num+" is not prime")
    }
  }
      
}