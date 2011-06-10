package gmrowe.projecteuler.util

trait Timeable[A] {
  def time(f: => A): (A, Long) = {
    val start = System.currentTimeMillis
    val res = f
    val end = System.currentTimeMillis
    (res, end - start)
  }
}

object Timeable {
   def time[A](f: => A): (A, Long) = new Timeable[A]{}.time(f)
   def timeSeconds[A](f: => A): (A, Double) = {
     val (res, t) = time(f)
     val millisPerSecond = 1000.0
     (res, t / millisPerSecond)
   }
}