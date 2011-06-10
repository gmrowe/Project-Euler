package gmrowe.projecteuler

import org.specs._
import EulerUtils._
import Problem38._

class Problem38Spec extends Specification {
   
   "Problem38.concat" should {
      "concatenate two numbers" in {
         (concat(12, 34) mustEqual 1234) &&
            (concat(1, 234) mustEqual 1234) &&
            (concat(123, 4) mustEqual 1234)
      }
   }
   
   "Problem38.largestPandigital" should {
      "return correctly for previously known result" in {
         (pandigitalProduct(192).get mustEqual (192384576)) &&
            (pandigitalProduct(9).get mustEqual (918273645))
      }
   }

}