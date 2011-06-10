package gmrowe.projecteuler

import EulerUtils._
import scala.math._

/**
 * The number, 1406357289, is a 0 to 9 pandigital number because it is made
 * up of each of the digits 0 to 9 in some order, but it also has a rather 
 * interesting sub-string divisibility property.
 *
 * Let d_(1) be the 1^(st) digit, d_(2) be the 2^(nd) digit, and so on. In
 * this way, we note the following:
 *
 *   * d_(2)d_(3)d_(4)=406 is divisible by 2
 *   * d_(3)d_(4)d_(5)=063 is divisible by 3
 *   * d_(4)d_(5)d_(6)=635 is divisible by 5
 *   * d_(5)d_(6)d_(7)=357 is divisible by 7
 *   * d_(6)d_(7)d_(8)=572 is divisible by 11
 *   * d_(7)d_(8)d_(9)=728 is divisible by 13
 *   * d_(8)d_(9)d_(10)=289 is divisible by 17
 *
 * Find the sum of all 0 to 9 pandigital numbers with this property.
 */
object Problem43 {

  def subnum(start: Int, end: Int)(n: BigInt): Int = {
    val base = BigInt(10)
    val numDigits = countDigits(n)
    if (start < 0 || start >= numDigits) {
      throw new IndexOutOfBoundsException(start.toString)
    } else if (start > end) {
      throw new IllegalArgumentException("start index > end index")
    } else {
      val endPow = if (end > numDigits) numDigits else end
      val begin = n / base.pow(numDigits - endPow)
      begin % base.pow(endPow - start) toInt
    }
  }
  
  //The subnum areguments are different than the example because we define
  //the semantics of subnum differently, in particular we define subnum
  //with substring semantics where the first argument is the index of the
  //first Int included in the result, and the second argument is the index
  //of the first Int excluded from the result.
  def hasUnusualProperty(n: BigInt): Boolean = {   
    subnum(1, 4)(n) % 2 == 0 &&
    subnum(2, 5)(n) % 3 == 0 &&
    subnum(3, 6)(n) % 5 == 0 &&
    subnum(4, 7)(n) % 7 == 0 &&
    subnum(5, 8)(n) % 11 == 0 &&
    subnum(6, 9)(n) % 13 == 0 &&
    subnum(7, 10)(n) % 17 == 0
  }
  
  //a b c d e f g h i j
  //1     0   5(0 6)
  //2     2    (1 7)
  //3     4    (2 8)
  //4     6    (3 9)
  //5     8    (6 1)
  //6          (7 2)
  //7          (8 3)
  //8          (9 4)
  //9
  def solve: List[BigInt] = {
    val pos0 = List(1, 2, 3, 4, 6, 7, 8)
    val pos3 = List(0, 2, 4, 6, 8)
    val pos6 = List(5)
    val pos7 = List(0, 1, 2, 3, 6, 7, 8, 9)
    val pos8 = List(1, 2, 3, 4, 6, 7, 8, 9)
    val perms = 
      for { i <- pos0
            j <- pos3 if j != i
            k <- pos6 if k != j && k != i
            l <- pos7 if l != k && l != j && l != i
            m <- pos8 if m != l && m != k && m != j && m != i
      } yield (i, j, k, l, m)
    
    def composeSpecial(list: List[Int], a: Int, d: Int, f: Int, g: Int, h: Int): BigInt = {
      val (mid1, rest0) = list splitAt 2
      val (mid2, rest) = rest0 splitAt 1
      composeBig((a :: mid1) ++ (d :: mid2) ++ (f :: g :: h :: rest))
    }
      
    val ps = 
      for { (a, b, c, d, e) <- perms 
             p <- permutations(List.range(0, 10) filter (n => n != a && n != b && n != c && n != d && n != e))
      } yield (composeSpecial(p, a, b, c, d, e))
    
    ps filter (hasUnusualProperty(_))
  }
  
  def main(args: Array[String]): Unit = {
    val (res, millis) = time(solve)
    println(res)
    println(res.sum)
    println((millis / 1000.0)+" seconds elapsed.")
  }
      
}

