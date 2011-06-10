package gmrowe.projecteuler

import Problem35._
import org.specs._
import EulerUtils._

class Problem35Spec extends Specification {
   "Problem35.allRotations" should {
      "return a singleton set when argument is a single digit" in {
         val n = 9
         val rs = allRotations(n)
         (rs.size mustEqual (1)) && 
            (rs must contain (n))
      }
      
      "return all rotations of argument value" in {
         val n = 1234
         val rs = allRotations(n)
         (rs.size mustEqual (4)) && 
            (rs must contain (n)) &&
            (rs must contain (2341)) &&
            (rs must contain (3412)) &&
            (rs must contain (4123))
      }
   }
}