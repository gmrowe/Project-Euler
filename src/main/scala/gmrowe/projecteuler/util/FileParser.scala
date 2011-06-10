package gmrowe.projecteuler.util

import java.io.{FileInputStream, IOException}
import java.util.Scanner
import scala.collection.mutable.ListBuffer

trait FileParser {

   def file: String
   
   def parseFiltered[A](filter: String => Boolean, parseFunc: String => A): List[A] = {
      try {
      
         val fileText = new Scanner(new FileInputStream(file))
            
         try {
         
            val listBuilder = new ListBuffer[A]
            while (fileText.hasNextLine) {
               val line = fileText.nextLine
               if (filter(line)) {
                  listBuilder += parseFunc(line)
               }
            }
            listBuilder.toList
            
         }  finally {
            fileText.close
         }
         
      } catch {
         case e: Exception => e.printStackTrace; List[A]()
      }
   }
   
   def parseAll[A](parseFunc: String => A): List[A] = {
      parseFiltered((_) => true, parseFunc)
   }
}

object FileParser {
  def fromPath(path: String): FileParser = new FileParser {
    val file = path
  }
}