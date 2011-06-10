package gmrowe.projecteuler.util

import java.io.{Writer, PrintWriter, FileWriter}
import scala.util.Random

class TestCaseGenerator {

   def shuffle(arr: Array[String]): Unit = {
   
      val len = arr.length
   
      def swap(from: Int, to: Int) {
         val temp = arr(from)
         arr(from) = arr(to)
         arr(to) = temp
      }
      
      def loop(cnt: Int) {
         if (cnt < len) {
            swap(cnt, randomGen.nextInt(len))
            loop(cnt + 1)
         }
      }
      
      loop(0)
   }

   def numCases: Int = 50000
   
   def separator: String = "\n"
   
   lazy val randomGen: Random = new Random
   
   def output: Writer = new FileWriter("test_cases.txt")
   
   private[this] lazy val cards: Array[String] = {     
      val ranks = Array[String]("A", "K", "Q", "J", "T", "9",
                                "8", "7", "6", "5", "4", "3", "2")
      val suits = Array[String]("S", "D", "C", "H")
      
      for (suit <- suits; rank <- ranks) yield (rank + suit)
   }
   
   def generateCase: String = {     
      shuffle(cards)
      val handSize = 10
      val hands = cards.take(handSize)
      val sb = new StringBuilder
      hands.foreach((hand) => sb.append(hand).append(" "))
      sb.toString.trim   
   }
   
   def generateCases: String = {
      val lb = new _root_.scala.collection.mutable.ListBuffer[String]
      
      def loop(cnt: Int) {
         if (cnt < numCases) {
            lb += generateCase
            loop(cnt + 1)
         }
      }
      
      loop(0)
      lb.mkString(separator)
   }
   
   def writeCases: Unit = {
      val writer = new PrintWriter(output)
      writer.println(generateCases)
      writer.flush
      writer.close
   }
         
}

object TCGen {
   def main(args: Array[String]) {
      (new TestCaseGenerator).writeCases
   }
}