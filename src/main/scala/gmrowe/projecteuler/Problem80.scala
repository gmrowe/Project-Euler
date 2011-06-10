package gmrowe.projecteuler

import scala.math._
import scala.math.BigDecimal
import EulerUtils._

object Problem80 {

  // def sqrt(n: Int)(epsilon: BigDecimal): BigDecimal = {
    // val nbd = BigDecimal(n)
    // def nextIteration(last: BigDecimal): BigDecimal = {
      // val next = (last + (nbd / last)) / BigDecimal(2)
      // val diff = (next - last).abs
      // if (diff <= epsilon) last else nextIteration(next)
    // }
    // val initGuess = BigDecimal(sqrtEstimate(n))
    // nextIteration(initGuess)
  // }

  def nextDigit(partialSolution: Int, target: Int): (Int, Int) = {
    def next(curr: Int): Int = {
      if (((partialSolution * 20) + curr) * curr > target) curr - 1
      else next(curr + 1)
    }
    val res = next(0)
    (res, target - res)
  }
  
  def sqrtEstimate(n: Int): Int = {
    def est(last: Int): Int = {
      val next = last + 1
      if (next * next > n) last else est(next)
    }
    if (n == 0) 0 else est(1)
  }

  def groupByTwos(n: Int): List[Int] = {
    def group(i: Int, acc: List[Int]): List[Int] = 
      if (i == 0) acc else group(i / 100, (i % 100)::acc)

    group(n, Nil)
  }

  def composeToDecimal(ls: (List[Int], List[Int])): Double = {
    def composeWhole(ls: List[Int]): Double = {
      def cmp(ls: List[Int], acc: Double): Double = ls match {
         case Nil => acc
         case x::xs => cmp(xs, (10.0 * acc) + x.toDouble)
      }
      cmp(ls, 0.0)
    }

    def composeFractional(ls: List[Int]): Double = {
      def cmp(ls: List[Int], acc: Double): Double = ls match {
        case Nil => acc
        case x::xs => cmp(xs, (x.toDouble + acc) / 10.0)
      }
      cmp(ls.reverse, 0.0)
    }
    composeWhole(ls _1) + composeFractional(ls _2)
  }

}