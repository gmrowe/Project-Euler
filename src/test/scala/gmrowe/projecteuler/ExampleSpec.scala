package gmrowe.projecteuler

import org.specs._
import org.specs.matcher.Matcher

class ExampleSpec extends Specification {
  def allSatisfy[A](m: => Matcher[A]): Matcher[Iterable[A]] = 
    new Matcher[Iterable[A]] {
      def apply(is: => Iterable[A]) = is find (e => !(m(e)._1)) match {
        case Some(e) => 
          (false, "", e+" doesn't satisfy requirement: ("+m(e)._3+")")
        case None => (true, "All elements satisfy the expectation", "")
      }
    }
    
  def notAllSatisfy[A](m: => Matcher[A]): Matcher[Iterable[A]] = allSatisfy(m).not

  "allSatisfy" should {
    "Pass if all elements satisfy the expectation" in {
      List(1, 2, 3, 4) must allSatisfy(beLessThan(5))
    }
    
    "Fail if any elements do not satisfy the expectation" in {
      List(1, 2, 3, 4, 5) must notAllSatisfy(beLessThan(5))
    }
  }
}
