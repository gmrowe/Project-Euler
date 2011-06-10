package gmrowe.projecteuler

import scala.io.Source
import scala.io.Source._

trait PrimeParser {
   
   def primeTxtFile: String
   
   private val src: Source = Source.fromString(primeTxtFile)
   
   def primes: Iterator[Int] = {
      for { l <- src.getLines()
            num <- l.split("""\s+""").iterator if !num.isEmpty
      } yield (Integer.parseInt(num))
   }
}

object PrimeMain {
   
   def main(args: Array[String]){ 
      val parser = new PrimeParser{ val primeTxtFile = """lib\1000primes.txt""" }
      println((parser.primes.take (100)).toList)
   }
}
       