package gmrowe.projecteuler

object Problem59 {
  lazy val allCombinations: Seq[String] = {
    for { 
      x <- 'a' to 'z'
      y <- 'a' to 'z'
      z <- 'a' to 'z'
    } yield {
      x.toString + y.toString + z.toString
    }
  }
}
