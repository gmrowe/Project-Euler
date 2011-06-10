package gmrowe.projecteuler

import org.specs._
import Problem69._

class Problem69Spec extends Specification {
  //"\'hello world\' has 11 characters" in {
  //   "hello world".size must be equalTo(11)
  //}
  //"\'hello world\' matches \'h.* w.*\'" in {
  //   "hello world" must be matching("h.* w.*")
  //}
  
  "Problem69.gcd" should {
    "return the greatest common divisor of the arguments" in {
      gcd(2, 1) mustEqual 1
      gcd(3, 9) mustEqual 3
      gcd (15, 10) mustEqual 5
    }
  }
  
  "Problem69.listCoprime" should {
    "list numbers coprime to the argument" in {
      listCoprime(2) must haveTheSameElementsAs(Seq(1))
      listCoprime(5) must haveTheSameElementsAs(Seq(1, 2, 3, 4))
      listCoprime(9) must haveTheSameElementsAs(Seq(1, 2, 4, 5, 7, 8))
    }
  }
      
}
