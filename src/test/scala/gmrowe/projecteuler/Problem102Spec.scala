package gmrowe.projecteuler

import org.specs._
import Problem102._
import EulerUtils._
import Rational._

class Problem102Spec extends Specification {
  "Slope" should {
    "provide a normalize method which returns slope in minimal form" in {
      Slope(2, 4).normalize mustEqual Slope(1, 2)
      Slope(-2, 2).normalize mustEqual Slope(-1, 1)
      Slope(0, 5).normalize mustEqual Slope(0, 1)
    }
    
    "provide an equals method that compares only the normalized slopes" in {
      (Slope(2, 4) == Slope(1, 2)) must beTrue
      (Slope(2, 2) == Slope(3, 3)) must beTrue
      (Slope(1, 3) == Slope(1, 3)) must beTrue
      (Slope(0, 1) == Slope(0, 1000)) must beTrue
    }
  }
  
  "Line" should {
    "Provide an apply method that returns the y-value for any x" in {
      val line = Line(Point(1, 1), Slope(1, 1))
      line(5) mustEqual Rational(5)
      line(0) mustEqual Rational(0)
    }
    "Provide an intersection method which provides the point of intersection " +
    "of two lines" in {
      val line1 = Line.fromPoints(Point(1, 1), Point(5, -1))
      val line2 = Line.fromPoints(Point(2, 1), Point(3, -3))
      (line1 intersection line2) must beSome(Point(Rational(15, 7),Rational(3, 7)))
    }
  }
  
  "Triangle" should {
    "Properly detect if the origin is within it's bounds" in {
      val abc = Triangle(Point(-340,495), Point(-153,-910), Point(835,-947))
      val xyz = Triangle(Point(-175,41), Point(-421,-714), Point(574,-645))
      abc.containsOrigin must beTrue
      xyz.containsOrigin must beFalse
    }
  }
}