package gmrowe.projecteuler

import EulerUtils._

class Rational(val numerator: Int, val denominator: Int) 
     extends Ordered[Rational] {
   
   def this(n: Int) = this(n, 1)
   
   lazy val lowestTerms: Rational = {
      val divisor = gcd(numerator, denominator)
      if (divisor == 1) this
      else new Rational(numerator / divisor, denominator / divisor)
   }
      
   lazy val reciprocal: Rational = new Rational(denominator, numerator)
   
   lazy val unary_~ : Rational = reciprocal
   
   lazy val unary_- : Rational = new Rational(-numerator, denominator)
   
   def add(r: Rational): Rational = {
      val denom = denominator * r.denominator
      val num = (numerator * r.denominator) + (r.numerator * denominator)
      new Rational(num, denom)
   }
   
   def subtract(r: Rational): Rational = add(-r)
   
   def -(r: Rational): Rational = subtract(r)
   
   def +(r: Rational): Rational = add(r)
   
   def multiply(r: Rational): Rational = {
      if (r.numerator == r.denominator) {
         this
      } else {
         val rat = new Rational(r.numerator * numerator,
              r.denominator * denominator)
         rat.lowestTerms
      }
   }

   def *(r: Rational): Rational = multiply(r)
   
   def divide(r: Rational): Rational = multiply(r.reciprocal)
   
   def /(r: Rational): Rational = divide(r)
   
   override def equals(other: Any): Boolean = {
      other match {
         case rat: Rational => {
            val lt = rat.lowestTerms
            lt.numerator == lowestTerms.numerator &&
               lt.denominator == lowestTerms.denominator
         }
         
         case _  => false
      }
   }      
   
   override def toString: String = 
        lowestTerms.numerator + "/" + lowestTerms.denominator
        
   override def hashCode: Int = {
      var hash = 0
      val prime = 31
      hash = hash + (prime * lowestTerms.numerator)
      hash = hash + (prime * lowestTerms.denominator)
      hash
   }
   
   def compare(that: Rational): Int = {
      val denom = lcm(denominator, that.denominator)
      val thisNum = (denom / denominator) * numerator
      val thatNum = (denom / that.denominator) * that.numerator
      thisNum.compare(thatNum)
   }
   
   def min(other: Rational): Rational = if (this <= other) this else other
   def max(other: Rational): Rational = if (this >= other) this else other
}

object Rational {
  implicit def IntToRational(i: Int): Rational = apply(i)
  
  def apply(n: Int, d: Int): Rational = 
   if (d == 0) sys.error("Rational with denominator 0 is undefined") 
   else new Rational(n, d)
   
  def apply(n: Int): Rational = apply(n, 1)
}
  