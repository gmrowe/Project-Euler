package gmrowe.projecteuler

import EulerUtils._

object Problem73 {

  def run(limit: Int): Unit = {
    val (res, secs) = util.Timeable.timeSeconds {
    var counts = 0 
    for { d <- (5 to limit)
          n <- (d / 3) + 1 to (d / 2)
    } {
      if (gcd(n, d) == 1) {
        counts += 1
      }
    }
    counts 
    }
    println("Limit="+limit+" answer="+res)
    println(secs+" seconds elapsed")
  }
  def main(args: Array[String]): Unit = {
    List(10000, 20000, 30000, 40000, 100000) foreach run
  }
}
  
          
