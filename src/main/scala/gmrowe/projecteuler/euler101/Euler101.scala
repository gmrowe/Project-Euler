package gmrowe.projecteuler.euler101

import Rational._
import scala.math._
object Euler101 {

  private def backsubstitute(matrix: Array[Array[Rational]]): List[Rational] = {
    var map = Map[Int,Rational]()
    val facs = new scala.collection.mutable.ListBuffer[Rational]
    for (i <- matrix.length - 1 to 0 by -1) {
      val coeffs = for (j <- i + 1 to matrix(i).length - 2) yield {
        map(j) * matrix(i)(j) }
      val sum = coeffs.foldRight (rational(0)) (_ + _)
      val res = matrix(i)(matrix(i).length - 1) - sum
      map = map + (i -> res)
      res +=: facs
    }
    facs.toList
  }

  private def multiplyRow(factor: Rational, row: Array[Rational]): Array[Rational] = {
    for (i <- row) yield (i * factor)
  }
      

  def solveLinear(matrix: Array[Array[Rational]]): List[Rational] = {
    for ( i <- matrix.indices ) {
       matrix(i) = multiplyRow(matrix(i)(i).reciprocal, matrix(i))
       val pivotArray = matrix(i)
       for (j <- (i + 1) to (matrix.length - 1)) {
         val factor = (matrix(j)(i) / pivotArray(i)) * rational(-1)
         val normalized = multiplyRow(factor, pivotArray)
         for (k <- matrix(j).indices) {
           matrix(j)(k) = matrix(j)(k) + normalized(k)
         }
       }
    }
    backsubstitute(matrix)
  }
  
  //1 - n  + n^(2) - n^(3) + n^(4) - n^(5) +
  //sn^(6) - n^(7) + n^(8) - n^(9) + n^(10)
  def f(n: Long): Long = {
    1 - 
    n +
    (n * n) -
    (n * n * n) +
    (n * n * n * n) -
    (n * n * n * n * n ) + 
    (n * n * n * n * n * n ) -
    (n * n * n * n * n * n * n ) +
    (n * n * n * n * n * n * n * n ) -
    (n * n * n * n * n * n * n * n * n ) +
    (n * n * n * n * n * n * n * n * n * n )
  }

  private def makeBOP(ins: List[Long]): Long => Long = {
    val points = List.range(1, ins.size + 1).zip (ins) 
    val matrixList = {
      for ((x, y) <- points) yield {
        List.tabulate(ins.length)(n => rational(pow(x, n).toLong)) ++ 
           List(rational(y))
      }
    }
    val matrix = (matrixList.map (ls => ls.toArray)).toArray
    val coeffs = solveLinear(matrix)
    val variables = coeffs.zipWithIndex
    (n: Long) => {
      val terms = variables.map { case (coef, exp) =>
        (pow(n, exp).toLong) * coef.numerator / coef.denominator
      }
      terms.foldLeft[Long] (0L) (_ + _)
    }    
  }
      
  
  private def divergance(f1: Long => Long, f2: Long => Long): Long = {
    def test(n: Long): Long = {
      if (f1(n) == f2(n)) test(n + 1L) else n
    }
    test(1)
  }

  def solve: Long = {
    val numValues = 10
    val values = (for (i <- 1 to numValues) yield f(i)).toList
    val fits = 
      for (i <- 1 to numValues) yield {
        val valuesUsed = values.take(i)
        val bop = makeBOP(valuesUsed)
        val firstDifferent = divergance(f, bop)
        bop(firstDifferent)
      }
    fits.reduceLeft (_ + _)
  }
 
  def main(args: Array[String]): Unit = {
    import gmrowe.projecteuler.util.Timeable.time
    val (res, millis) = time(solve)
    println(res)
    println((millis / 1000.0)+" seconds")
  }
}