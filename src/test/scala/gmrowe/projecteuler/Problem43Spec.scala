package gmrowe.projecteuler

import Problem43._
import org.specs._
import EulerUtils._

class Problem43Spec extends Specification {

   "Problem43.subnum" should {
     "extract the subnum of an arbitrarily length number" in {
       subnum(0, 1)(1) mustEqual 1
       subnum(0, 1)(23) mustEqual 2
       subnum(0, 2)(456) mustEqual 45
       //subnum(1, 4)(1234) mustEqual 234
     }
   }
   
   "Problem43.hasUnusualProperty" should {
     "return true for a number known to exhibit the property" in {
       hasUnusualProperty(1406357289) must beTrue
     }
   }
}