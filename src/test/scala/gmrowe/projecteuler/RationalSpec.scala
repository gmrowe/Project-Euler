package gmrowe.projecteuler

import org.specs._
import EulerUtils._

class RationalSpec extends Specification {
   
   "Rational.lowestTerms" should {
      "return itself if already in lowest terms" in {
         val rat = new Rational(1, 2)
         rat.lowestTerms must be (rat)
      }
         
      "return a Rational in lowest terms if not in lowest terms" in {
         val rat = new Rational(4, 8)
         val lt = rat.lowestTerms
         (lt mustEqual (new Rational(1, 2))) &&
           (lt.lowestTerms must be (lt))
      }
      
      "return a positive rational if numerator and denominator are negative" in {
         new Rational(-1, -2).lowestTerms mustEqual (new Rational(1, 2))
      }
   }
      
   "Rational.multiply" should {
      "return itself if multiplied by unity" in {
         val rat = new Rational(2, 12)
         val x = 42
         val unity = new Rational(x, x)
         rat * unity mustEqual (rat)
      }
         
      "return a Rational obtained by multiplying by the argument" in {
         val orig = new Rational(1, 2) 
         val mult = new Rational(1, 3)
         orig * mult mustEqual (new Rational(1, 6))
      }
   }
      
   "Rational.add" should {
      "correctly add two rationals" in {
         new Rational(1, 3) + (new Rational(1, 3)) mustEqual (new Rational(2, 3))
      }
   }
      
   "Rational.reciprocal" should {
      "return the reciprocal Rational" in {
         val rat = new Rational(1, 3)
         (~rat mustEqual (new Rational(3, 1))) &&
            (rat * ~rat mustEqual (new Rational(1)))
      }
   }
}