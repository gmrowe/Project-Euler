package gmrowe.projecteuler

import org.specs._
import Problem59._

class Problem59Spec extends Specification {
  //"\'hello world\' has 11 characters" in {
  //   "hello world".size must be equalTo(11)
  //}
  //"\'hello world\' matches \'h.* w.*\'" in {
  //   "hello world" must be matching("h.* w.*")
  //}
  
  "Problem59.allCombinations" should {
    "produce a stream of all lower case three-letter combos" in {
      allCombinations.toSet.size must be equalTo(26 * 26 * 26)
    }
  }
}
