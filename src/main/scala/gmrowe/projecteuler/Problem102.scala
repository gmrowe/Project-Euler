package gmrowe.projecteuler

import EulerUtils._
import Rational._

object Problem102 {
  case class Point(x: Rational, y: Rational)
  
  class Line(val point: Point, val slope: Slope) extends (Rational => Rational) {
    lazy val normalize: Line = Line(Point(0, yIntercept), slope.normalize)
    
    lazy val yIntercept = apply(0)
    
    //x = (y1 - y0 - m1x1 + m0x0) / (m0 - m1)
    def intersection(o: Line): Option[Point] = {
      if (slope == o.slope) None
      else {
        val xi = (o.point.y  - point.y - (o.slope.asRational * o.point.x) +
         (slope.asRational * point.x)) / (slope.asRational - o.slope.asRational)
        Some(Point(xi, apply(xi)))
      }
    }
    //y = m(x - x0) + y0
    def apply(x: Rational): Rational = 
      slope.asRational * (x - point.x) + point.y
    
    override def equals(a: Any): Boolean = a match {
      case o: Line =>
        val oNorm = o.normalize
        (normalize.point == oNorm.point) && (normalize.slope == oNorm.slope)
      case _ => false
    }
  }
  
  object Line {
    def apply(p: Point, s: Slope) = new Line(p, s)
    def fromPoints(p1: Point, p2: Point): Line = {
      val slope = (p1.y - p2.y) / (p1.x - p2.x)
      Line(p1, Slope(slope.numerator, slope.denominator)).normalize
    }
  }

  case class Slope(rise: Int, run: Int) {
    private val rat = Rational(rise, run)
    lazy val normalize: Slope = {
      Slope(rat.lowestTerms.numerator, rat.lowestTerms.denominator)
    }
    
    val asRational: Rational = rat
    
    override def equals(a: Any): Boolean = a match {
      case o: Slope =>
        val oNorm = o.normalize
        (normalize.rise == oNorm.rise) && (normalize.run == oNorm.run)
      case _ => false
    }
  }
  
  case class Triangle(p1: Point, p2: Point, p3: Point) {
    val containsOrigin: Boolean = {
      def isVertical(a: Point, b: Point) = a.x == b.x
      val xAxis = Line(Point(0,0), Slope(0, 1))
      val segments = List((p1, p2), (p1, p3), (p2, p3))
      val crosses = segments count  { case (a, b) =>
        if (isVertical(a, b)) b.x > 0 && (a.y min b.y) <= 0 && (a.y max b.y) > 0
        else {
          val line = Line.fromPoints(a, b)
          line intersection xAxis match {
            case Some(p) => 
              p.x > 0 && p.x >= (a.x min b.x) && p.x <= (a.x max b.x)
            case None => false
          }
        }
      }
      crosses == 1
    }
  }
  
  def parseTriangleFile(path: String): List[Triangle] = 
    util.FileParser.fromPath(path) parseAll { s =>
      val nums = (s split (",") toList) map (i => i.toInt)
      nums match {
        case (p1X::p1Y::p2X::p2Y::p3X::p3Y::_) =>
          Triangle(Point(p1X, p1Y), Point(p2X, p2Y), Point(p3X, p3Y))
        case _ => sys.error("Parse error parsing Ttiangle file")
      }
    }
    
  def main(args: Array[String]): Unit = {
    val path = "src\\main\\resources\\triangles.txt"
    val (res, secs) = util.Timeable.timeSeconds {
      parseTriangleFile(path) count (t => t.containsOrigin)
    }
    println(res)
    println(secs+" seconds elapsed")
  }
}
