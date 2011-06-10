package gmrowe.projecteuler

import EulerUtils._
import scala.math._

object Problem44 {
  
  def integerRoot(n: Long): Long = {
    val root = math.sqrt(n)
    val flr = math.floor(root)
    math.round(flr)
  } 
  
  def isSquare(x: Long): Boolean = {
    val rt = integerRoot(x)
    rt * rt == x
  } 
  
  def isPentagonal(n: Long): Boolean = {
    val d = (24 * n) + 1
    isSquare(d) && ((integerRoot(d) + 1L) % 6L == 0)
  }
  
  def pentagonal(n: Long) = (n * ((3 * n) - 1)) / 2
  
  def sumIsPentagonal(a: Long, b: Long): Boolean = isPentagonal(a + b)
  def diffIsPentagonal(a: Long, b: Long): Boolean = isPentagonal(math.abs(a - b))
  
  def solve: Long = {
    val diffs = {
      for { i <- 1L to 10000000L
            n <- 1L to 50L
            val j =  pentagonal(i)
            val k =  pentagonal(i + n)
            if sumIsPentagonal(j, k) && diffIsPentagonal(j, k)
      } yield (math.abs(j - k))
    }
    diffs.min
  }
  
  def main(args: Array[String]): Unit = {
    import util.Timeable
    val (res, secs) = Timeable.timeSeconds(solve)
    println("Result = %s%n%.3f seconds elapsed".format(res, secs))
  }
  
}