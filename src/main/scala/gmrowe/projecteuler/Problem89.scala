package gmrowe.projecteuler

/**
  The rules for writing Roman numerals allow for many ways of writing each 
  number (see FAQ: Roman Numerals). However, there is always a "best" way of
  writing a particular number.

  For example, the following represent all of the legitimate ways of writing
  the number sixteen:

  IIIIIIIIIIIIIIII
  VIIIIIIIIIII
  VVIIIIII
  XIIIIII
  VVVI
  XVI

  The last example being considered the most efficient, as it uses the least
  number of numerals.

  The 11K text file, roman.txt (right click and 'Save Link/Target As...'), contains
  one thousand numbers written in valid, but not necessarily minimal, Roman
  numerals; that is, they are arranged in descending units and obey the
  subtractive pair rule (see FAQ for the definitive rules for this problem).

  Find the number of characters saved by writing each of these in their minimal form.
*/


object Problem89 {
   
   def decodeRoman(rn: String): Int = {
      def decode(rn: Seq[Char], acc: Int): Int = rn match {
         case Seq()                    => acc
         case Seq('I', 'V', rest @ _*) => decode(rest, acc + 4)
         case Seq('I', 'X', rest @ _*) => decode(rest, acc + 9)
         case Seq('X', 'L', rest @ _*) => decode(rest, acc + 40)
         case Seq('X', 'C', rest @ _*) => decode(rest, acc + 90)
         case Seq('C', 'D', rest @ _*) => decode(rest, acc + 400)
         case Seq('C', 'M', rest @ _*) => decode(rest, acc + 900)
         case Seq('I', rest @ _*) => decode(rest, acc + 1)
         case Seq('V', rest @ _*) => decode(rest, acc + 5)
         case Seq('X', rest @ _*) => decode(rest, acc + 10)
         case Seq('L', rest @ _*) => decode(rest, acc + 50)
         case Seq('C', rest @ _*) => decode(rest, acc + 100)
         case Seq('D', rest @ _*) => decode(rest, acc + 500)
         case Seq('M', rest @ _*) => decode(rest, acc + 1000)
      }
      decode(rn.toUpperCase, 0)
   }
   
   def encodeRoman(n: Int): String = {
      def encode(n: Int, acc: StringBuilder): String = n match {
         case _ if n >= 1000 => encode(n - 1000, acc append "M")
         case _ if n >= 900 => encode(n - 900, acc append "CM")
         case _ if n >= 500 => encode(n - 500, acc append "D")
         case _ if n >= 400 => encode(n - 400, acc append "CD")
         case _ if n >= 100 => encode(n - 100, acc append "C")
         case _ if n >= 90 => encode(n - 90, acc append "XC")
         case _ if n >= 50 => encode(n - 50, acc append "L")
         case _ if n >= 40 => encode(n - 40, acc append "XL")
         case _ if n >= 10 => encode(n - 10, acc append "X")
         case _ if n >= 9 => encode(n - 9, acc append "IX")
         case _ if n >= 5 => encode(n - 5, acc append "V")
         case _ if n >= 4 => encode(n - 4, acc append "IV")
         case _ if n >= 1 => encode(n - 1, acc append "I")
         case _ if n == 0 => acc.toString
      }
      encode(n, new StringBuilder)
   }
   
   val location = "src\\main\\resources\\roman.txt"
   
   import scala.io.Source
   import Source._
   def lines: List[String] = {
      val src = Source.fromString(location)
      src.getLines().toList
   }
   
   def solve: Int = {
     val diffs =  for (line <- lines) yield {
        line.length - encodeRoman(decodeRoman(line)).length
     }
     diffs.sum
   }
     
   
   import util.Timeable.time
   def main(args: Array[String]) {
      println(time(solve))
   }


}