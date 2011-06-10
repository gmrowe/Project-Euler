package gmrowe.projecteuler

import Problem40._
import org.specs._
import EulerUtils._

class Problem40Spec extends Specification {

   "Problem40.rptIdx" should {
      "do the right thing" in {
         rptIdx(12) mustEqual 1
      }
   }

}