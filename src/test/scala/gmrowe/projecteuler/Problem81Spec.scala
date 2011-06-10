package gmrowe.projecteuler

import org.specs._
import Problem81._
import util.FileUtils._


class Problem81Spec extends Specification {
  "Problem81" should {
    "Parse a text file of lines of comma separated ints to a " +
    "Seq[Seq[Int]]" in {
      val path = "src" \ "test" \ "resources" \ "problem81_test-input.txt"
      parseMatrix(path) must beEqualTo(Seq(Seq(1,2,3),Seq(4,5,6),Seq(7,8,9)))
    }    
    "Find the minimal path from top-left to bottom-right of a matrix" in {
      val path = "src" \ "main" \ "resources" \ "problem81-small_input.txt"
      val matrix = parseMatrix(path)
      minPath(matrix) must beEqualTo(List(131,201,96,342,746,422,121,37,331))
    }    
  }
}