package gmrowe.projecteuler.euler101

trait Rational {
   def lt: Rational
   def +(other: Rational): Rational
   def -(other: Rational): Rational
   def *(other: Rational): Rational
   def /(other: Rational): Rational
   def reciprocal: Rational
   def numerator: Long
   def denominator: Long
}

object Rational {
  
  def rational(n: Long, d: Long): Rational = {
    if (d == 0) error("zero denominator")
    else if (n == 0) Zero
    else new NonZeroRational(n, d)
  }

  def rational(whole: Long): Rational = rational(whole, 1)
    
  private case object Zero extends Rational {
    def lt: Rational = this
    def +(o: Rational): Rational = o
    def -(o: Rational): Rational = {
      if (o == this) this
      else rational(-o.numerator, o.denominator)
    }
    def *(o: Rational): Rational = this
    def /(o: Rational): Rational = this
    def reciprocal: Rational = error("reciprocal of zero is undefined")
    def numerator: Long = 0
    def denominator: Long = 1
    override def toString: String = "Rational(0)"
  }

  private class NonZeroRational(val numerator: Long, val denominator: Long)
       extends Rational {
    private def gcd(a: Long, b: Long): Long = 
      if (b == 0) a else gcd(b, a % b)

    private def lcm(a: Long, b: Long): Long =
      (a / gcd(a, b)) * b

    lazy val lt: Rational = {
      val divisor = gcd(denominator, numerator)
      val num = numerator / divisor
      val denom = denominator / divisor
      if (denom < 0) {
         new NonZeroRational(-num, -denom)
      } else {
         new NonZeroRational(num, denom)
      }
    }

    def +(o: Rational): Rational = o match {
      case Zero => this
      case _    => 
        val mult = lcm(denominator, o.denominator)
        val term1 = (mult / denominator) * numerator
        val term2 = (mult / o.denominator) * o.numerator       
        rational(term1 + term2, mult).lt
    }
    def -(o: Rational): Rational = o match {
      case Zero => this
      case _    => 
        val mult = lcm(denominator, o.denominator)
        val term1 = (mult / denominator) * numerator
        val term2 = (mult / o.denominator) * o.numerator
        rational(term1 - term2, mult).lt
    }

    def *(o: Rational): Rational = {
      rational(numerator * o.numerator, denominator * o.denominator)
    }    

    def /(o: Rational): Rational = o match {
      case Zero => error("division by zero")
      case _    => rational(numerator * o.denominator,
           denominator * o.numerator)
    }
 
    def reciprocal: Rational = new NonZeroRational(denominator, numerator)

    override def equals(a: Any): Boolean = a match {
      case o: NonZeroRational =>
        o.lt.denominator == lt.denominator &&
        o.lt.numerator == lt.numerator
      case _  => false
    }

    override def toString: String = {
      if (lt.denominator == 1) "Rational("+lt.numerator+")"
      else "Rational("+lt.numerator+"/"+lt.denominator+")"
    }
    
    override def hashCode: Int = {
      var hash = 0L
      val prime = 31L
      hash = hash + (prime * lt.numerator)
      hash = hash + (prime * lt.denominator)
      hash.toInt
    }
   
    def compare(that: Rational): Int = {
      val denom = lcm(denominator, that.denominator)
      val thisNum = (denom / denominator) * numerator
      val thatNum = (denom / that.denominator) * that.numerator
      thisNum.compare(thatNum)
    } 
  }
}
  