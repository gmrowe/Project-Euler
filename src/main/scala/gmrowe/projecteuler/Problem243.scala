package gmrowe.projecteuler
import EulerUtils._

object Problem243 {
  private val precompLimit = 10000
  private val primes = primesBelow(precompLimit)
  private val lastPrecomped = primes.last
  
  def admissable(n: Int): Boolean = {
    isPowerOf2(n) || distinctPrimeFactorsAreConsecutivePrimes(n)
  }

  def isPowerOf2(n: Int): Boolean = {
    n > 0 && (n & (n - 1)) == 0
  }
  
  def distinctPrimeFactorsAreConsecutivePrimes(n: Int): Boolean = {
    val factors = List(distinctPrimeFactors(n).toSeq: _*).sorted
    areConsecutivePrimes(factors)
  }
    
  
  def areConsecutivePrimes(ns: List[Int]): Boolean = {
    primes containsSlice ns
    
  }
  
  def primeAfter(n: Int): Int = {
    primes dropWhile (_ <=  n) head
  }
      
}
