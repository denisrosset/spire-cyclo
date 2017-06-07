package cyclo

import spire.math.Rational
import spire.util.Opt

/** Represents a member of a quadratic field, of value a + b * sqrt(d) where
  *
  * @param a Rational part
  * @param b Coefficient of the sqrt
  * @param d Nonnegative square-free integer
  */
case class Quadratic(a: Rational, b: Rational, d: Int) {
  def toCyclo: Cyclo = Cyclo(a) + b * Cyclo.sqrt(d)
  def toRealCyclo: RealCyclo = RealCyclo(a) + RealCyclo(b) * RealCyclo.sqrt(d)
  def print(sb: StringBuilder): Unit = {
    val hasConstant = a.signum match {
      case -1 =>
        sb ++= "-"
        sb ++= a.abs.toString
        true
      case 1 =>
        sb ++= a.toString
        true
      case _ => false // case 0
    }
    val hasSqrt = b.signum match {
      case -1 =>
        sb ++= "-"
        val numAbs = -b.numerator
        if (!numAbs.isOne) {
          sb ++= numAbs.toString
          sb ++= "*"
        }
        true
      case 1 =>
        if (hasConstant) sb ++= "+"
        if (!b.numerator.isOne) {
          sb ++= b.numerator.toString
          sb ++= "*"
        }
        true
      case _ => false // case 0
    }
    if (hasSqrt) {
      sb ++= "sqrt("
      sb ++= d.toString
      sb ++= ")"
      if (!b.denominator.isOne) {
        sb ++= "/"
        sb ++= b.denominator.toString
      }
    }
  }
}

object Quadratic {

  /** Returns, if it exists, a decomposition of the given cyclotomic number into a member
    * of a quadratic field.
    */
  def fromCycloOpt(x: Cyclo): Opt[Quadratic] = x.order match {
    case 1 => Opt(Quadratic(x.toRational, Rational.zero, 1))
    case d =>
      val sd = Cyclo.sqrt(d)
      if (sd.order != x.order) return Opt.empty[Quadratic]
      val nonZeroExp = if (sd.exponent(0) != 0) sd.exponent(0) else sd.exponent(1)
      val b = x.coefficientForExponent(nonZeroExp) / sd.coefficientForExponent(nonZeroExp)
      val a = x - sd * b
      val factors = Factors(d)
      a.toRationalOpt match {
        case Opt(ra) => Opt(Quadratic(ra, b * factors.squarePartSqrt, factors.squareFreePart))
        case _ => Opt.empty[Quadratic]
      }
  }

}