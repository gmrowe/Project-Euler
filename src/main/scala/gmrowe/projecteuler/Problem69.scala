package gmrowe.projecteuler

object Problem69 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  
  def listCoprime(n: Int): Seq[Int] = (1 until n) filter (x => gcd(n, x) == 1)
  
  def totient(n: Int): Double = n.toDouble / listCoprime(n).size
  
  def solve(max: Int): Int = {
    val zipped = (2 to max) map (n => (n, totient(n)))
    val maxPair = zipped reduceLeft ((currMax, curr) => (currMax, curr) match {
      case ((n0, t0), (n1, t1)) => if (t1 > t0) curr else currMax
    })
    maxPair._1
  }
  
  def main(args: Array[String]): Unit = {
    println(solve(1000000))
  }
}
