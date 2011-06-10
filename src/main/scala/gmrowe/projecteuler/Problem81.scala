package gmrowe.projecteuler
import scala.collection.immutable.IndexedSeq
import util.FileUtils._
import util.Timeable._

/**In the 5 by 5 matrix below, the minimal path sum from the top left to the
  *bottom right, by only moving to the right and down, is indicated in bold red
  *and is equal to 2427.
  *    131	673	234	103	18
  *    201	96	342	965	150
  *    630	803	746	422	111
  *    537	699	497	121	956
  *    805	732	524	37	331
	*Find the minimal path sum, in matrix.txt (right click and
	*'Save Link/Target As...'), a 31K text file containing a 80 by 80 matrix,
	*from the top left to the bottom right by only moving right and down.
	*(matrix.txt can be found at src\main\resources\matrix.txt)
	*
	*The algorithm used is based upon the fact that for a matrix:
  *  a b c
  *  d e f
  *  g h i
  *The min path from 'a' depends only on the min path from 'b' and 'd'. And
  *working backward, the shortest path from 'i' is the trivial empty path. 
  *Further there is only one path to 'i' from anyplace on the bottom row, 
  *likewise, there is only one path to i from anyplace on the leftmost row
  *So for the first place where there is a choice of paths 'e' in our example
  *we already have enough information encoded in the 'f' and 'h' positions to
  *choose the optimal path in constant time. Working backward in this fashion
  *we can only have 2 paths to choose from by the time we get to the origin*/
object Problem81 {
          
  def main(args: Array[String]): Unit = {
    val path = "src" \ "main" \ "resources" \ "matrix81.txt"
    val matrix = parseMatrix(path)
    val (res, t) = timeSeconds(minPath(matrix))
    println("Result: "+res.sum)
    println(t+" seconds elapsed")
  }

  def parseMatrix(path: String): Seq[Seq[Int]] = {
    val parser = new util.FileParser { val file = path }
    parser.parseAll {line =>
      val tokens = line split ","
      tokens map (t => t.toInt) toIndexedSeq
    }  toIndexedSeq
  }
  
  def minPath(matrix: Seq[Seq[Int]]): List[Int] = {
    var m = matrix map (row => row map (n => (n, List[Int]())))
    for { i <- (m.length - 1) to 0 by -1
          j <- (m(i).length - 1) to 0 by -1
    } {
      val (currVal, _) = m(i)(j)
      val (minVal, minPath) = {
        if (m.isDefinedAt(i + 1) && m(i).isDefinedAt(j + 1)) {
          val b@(botVal, _) = m(i + 1)(j)
          val l@(lefVal, _) = m(i)(j + 1)
          if (botVal < lefVal) b else l
        } else if (m.isDefinedAt(i + 1) && !m(i).isDefinedAt(j + 1)) {
          m(i + 1)(j)
        } else if (!m.isDefinedAt(i + 1) && m(i).isDefinedAt(j + 1)) {
          m(i)(j + 1)
        } else {
          (0, Nil)
        }
      }
      m = m.updated(i, m(i).updated(j, (currVal + minVal, currVal::minPath)))
    }
    m(0)(0) _2  
  }

}

