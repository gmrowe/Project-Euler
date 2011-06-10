package gmrowe.projecteuler.util

object FileUtils {  
  implicit def StringToStrings(s: String)  = new Strings(s)
}

class Strings(pathPart: String) {
  val sep = System.getProperty("file.separator")
  
  def /(nextPart: String): String = pathPart + sep + nextPart
  def \(nextPart: String): String = /(nextPart)
}

