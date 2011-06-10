package gmrowe.projecteuler

import org.specs._
import Problem101._

class Problem101Spec extends Specification {
   "Problem101.swapPos" should {
     "swap element positions in a List" in {
       swapPos(List(1, 2, 3), 0, 2) mustEqual List(3, 2, 1)
     }
     "be agnostic about index order" in {
       swapPos(List(1, 2, 3), 2, 0) mustEqual List(3, 2, 1)
     }
   }
   
   
}