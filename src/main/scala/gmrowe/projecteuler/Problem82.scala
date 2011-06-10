package gmrowe.projecteuler
import scala.collection.immutable.IndexedSeq
import util.FileUtils._
import util.Timeable._
/**NOTE: This problem is a more challenging version of Problem 81.
  *The minimal path sum in the 5 by 5 matrix below, by starting in any cell in
  *the left column and finishing in any cell in the right column, and only
  *moving up, down, and right, is indicated in red and bold; the sum is equal to
  *994.
	*
  *  131	673	234	103	18
  *  201	96	342	965	150
  *  630	803	746	422	111
  *  537	699	497	121	956
  *  805	732	524	37	331
	*
  *Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target
  *As...'), a 31K text file containing a 80 by 80 matrix, from the left column
  *to the right column.*/
object Problem82 {
          
  def main(args: Array[String]): Unit = {
    val path = "src" \ "main" \ "resources" \ "matrix82.txt"
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
  
  case class PathElement(value: Int, path: List[Int], complete: Boolean)
  
  def seedLastColumn(i: Seq[Seq[PathElement]]): Seq[Seq[PathElement]] = {
    var seeded = i
    for (row <- seeded.indices) {
      val lastIdx = i(row).size - 1
      val PathElement(old, _, _) = seeded(row)(lastIdx)
      seeded = seeded.updated(row, seeded(row).updated(lastIdx, PathElement(old, List(old), true)))
    }
    seeded
  }
  
  def initColumn(column: Int, matx: Seq[Seq[PathElement]]): 
    Seq[Seq[PathElement]] = {
      var m = matx
      for (row <- matx.indices) {
        val PathElement(rightVal, rightPath, _) = m(row)(column + 1)
        val PathElement(currVal, _, _) = m(row)(column)
        m = m.updated(row, m(row).updated(column, PathElement(currVal + rightVal, currVal::rightPath, false)))
      }
      m
  }
    
  def allPathsComplete(column: Int, matx: Seq[Seq[PathElement]]): Boolean = {
    val completes =  for (row <- matx.indices) yield {
        val PathElement(_, _, complete) = matx(row)(column)
        complete
    }
    completes.forall (b => b)
  }
  
  def complete(pe: PathElement): Boolean = pe match {
    case PathElement(_, _, completed) => completed
  }
    
    
  def lowestIncompleteRow(col: Int, matx: Seq[Seq[PathElement]]): Int = {
    val irs = for (row <- matx.indices if !complete(matx(row)(col))) yield row
    irs.reduceLeft {(row1, row2) => (matx(row1)(col), matx(row2)(col)) match {
      case (PathElement(v1, _, _), PathElement(v2, _, _)) =>
        if (v1 < v2) row1 else row2
      }
    }
  }
    
  def bestPath(col: Int, row: Int, matx: Seq[Seq[PathElement]]):
    Seq[Seq[PathElement]] = {
    val PathElement(currVal, currPath, _) = matx(row)(col)
    val elem = currPath.head
    val above = 
      if (matx.isDefinedAt(row - 1) && complete(matx(row - 1)(col))){
        Some(matx(row - 1)(col))
      } else{
        None
      }
      
    val below =
      if (matx.isDefinedAt(row + 1) && complete(matx(row + 1)(col))){
        Some(matx(row + 1)(col))
      } else{
        None
      }
      
    (above, below) match {
      case (Some(p1), Some(p2)) => (p1, p2) match {
        case (PathElement(v1, p1, _), PathElement(v2, p2, _)) =>
          if (v1 < v2 && v1 < currVal) matx.updated(row, matx(row).updated(col, PathElement(elem + v1, elem::p1, true)))
          else if (v2 < v1 && v2 < currVal) matx.updated(row, matx(row).updated(col, PathElement(elem + v2, elem::p2, true)))
          else matx.updated(row, matx(row).updated(col, PathElement(currVal, currPath, true)))
      }
      case (Some(p1), None) => p1 match {
        case PathElement(v1, p1, _) => 
          if (v1 < currVal) matx.updated(row, matx(row).updated(col, PathElement(elem + v1, elem::p1, true)))
          else matx.updated(row, matx(row).updated(col, PathElement(currVal, currPath, true)))
      }
      case (None, Some(p2)) => p2 match {
        case PathElement(v1, p1, _) => 
          if (v1 < currVal) matx.updated(row, matx(row).updated(col, PathElement(elem + v1, elem::p1, true)))
          else matx.updated(row, matx(row).updated(col, PathElement(currVal, currPath, true)))
      }
      case (None, None) => matx.updated(row, matx(row).updated(col, PathElement(currVal, currPath, true)))
    }
  }
  def minPath0(matx: Seq[Seq[PathElement]]): List[Int] = {
    val min = matx.reduceLeft {(row1, row2) => (row1(0), row2(0)) match {
      case (PathElement(v1, _, _), PathElement(v2, _, _)) =>
        if (v1 < v2) row1 else row2
      }
    }
    val PathElement(_, path, _) = min(0)
    path
  }
    
  
  //init last column to it's own value
  //for each column from (r - 1) to l
    //init each value to curr + vslue to the immediate right
    //until all paths are marked as complete
      //find lowest unmarked path
        //if it is adjacent to one or more completed paths, choose lowest path
        //else the current path is the lowest path
        //mark as complete
  def minPath(matrix: Seq[Seq[Int]]): List[Int] = {
     val init = matrix map (row => row map (n => PathElement(n, List(), false)))
     var seeded = seedLastColumn(init)
     for (col <- (seeded(0).size - 2) to 0 by -1) {
       seeded = initColumn(col, seeded)
       while(!allPathsComplete(col, seeded)) {
         val lowRow = lowestIncompleteRow(col, seeded)
         seeded = bestPath(col, lowRow, seeded)
       }
     }
     minPath0(seeded)        
  }
  
}
