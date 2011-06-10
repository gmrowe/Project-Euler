package gmrowe.projecteuler

import EulerUtils._

object Problem42 {
   
   trait WordParser {
   
      import scala.io.Source
      import scala.io.Source._
      
      def txtFile: String
      
      private val src: Source = Source.fromString(txtFile)
      
      def words: Iterator[String] = {
         val line = src.getLines.toIndexedSeq(1)
         val ws = line.split(',')
         ws.iterator.map (s => s.substring(1, s.length - 1))
      }
   }
   

   val triangleNums: Stream[Int] = {
      def tn(n: Int): Stream[Int] = {
         Stream.cons((n * (n + 1)) / 2, tn(n + 1))
      }
      tn(1)
   }
   
   val letterMap: Map[Char, Int] = {
      val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toArray.zipWithIndex
      letters.foldLeft (Map.empty[Char, Int]) ((m, tup) => m + (tup._1 -> (tup._2 + 1)))
   }
   
   def scoreWord(word: String): Int = {         
      word.foldLeft (0) ((total, chr) => total + letterMap(chr))
   }
   
   def isTriangleNum(num: Int): Boolean = {
      val nums = triangleNums.takeWhile (_ <= num) force;
      nums.last == num
   }
   
   def isTriangleWord(word: String): Boolean = isTriangleNum(scoreWord(word))
   
   lazy val solve: Int = {
      val parser = new WordParser { val txtFile = """lib\words.txt""" }
      val tWords = parser.words.filter (isTriangleWord)
      tWords.foldLeft (0) ((cnt, x) => cnt + 1)
   }
   
   def main(args: Array[String]) {
      val elapsed = time(println(solve))
      println(elapsed + "ms elapsed")
   }

}