package gmrowe.projecteuler

import org.specs._
import Problem89._

class Problem89Spec extends Specification("Roman Numeral Spec") {
   
   "decodeRoman" should {
      "parse simple numbers" in {
         decodeRoman("I") mustEqual 1
         decodeRoman("V") mustEqual 5
         decodeRoman("X") mustEqual 10
         decodeRoman("L") mustEqual 50
         decodeRoman("C") mustEqual 100
         decodeRoman("D") mustEqual 500
         decodeRoman("M") mustEqual 1000
      }
      
      "parse valid subtractive combinations" in {
         decodeRoman("IV") mustEqual 4
         decodeRoman("IX") mustEqual 9
         decodeRoman("XL") mustEqual 40
         decodeRoman("XC") mustEqual 90
         decodeRoman("CD") mustEqual 400
         decodeRoman("CM") mustEqual 900
      }
      
      "parse equivalent representations to the same result" in {
         val expected = 16
         decodeRoman("IIIIIIIIIIIIIIII") mustEqual expected
         decodeRoman("VIIIIIIIIIII") mustEqual expected
         decodeRoman("VVIIIIII") mustEqual expected
         decodeRoman("XIIIIII") mustEqual expected
         decodeRoman("VVVI") mustEqual expected
         decodeRoman("XVI") mustEqual expected
      }
      
      "parse equivalent representations with subtractive " +
      "combinations to the same result" in {
         val expected = 49
         decodeRoman("XXXXVIIII") mustEqual expected
         decodeRoman("XXXXIX") mustEqual expected
         decodeRoman("XLVIIII") mustEqual expected
         decodeRoman("XLIX") mustEqual expected
      }
   }
   
   "encodeRoman" should {
      "encode simple numbers" in {
         encodeRoman(0) mustEqual ""
         encodeRoman(1) mustEqual "I"
         encodeRoman(5) mustEqual "V"
         encodeRoman(10) mustEqual "X"
         encodeRoman(50) mustEqual "L"
         encodeRoman(100) mustEqual "C"
         encodeRoman(500) mustEqual "D"
         encodeRoman(1000) mustEqual "M"
      }
      
      "encode a minimal-form roman numeral representation" in {
         encodeRoman(16) mustEqual ("XVI")
         encodeRoman(49) mustEqual ("XLIX")
         encodeRoman(1606) mustEqual ("MDCVI")
      }
      
      "be complementary to decodeRoman" in {
         (1 to 1000).map (n => decodeRoman(encodeRoman(n)) mustEqual n)
      }
   }
         
}