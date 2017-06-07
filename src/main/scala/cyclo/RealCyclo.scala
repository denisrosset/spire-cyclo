package cyclo

import java.math.RoundingMode

import spire.algebra._
import spire.math.{Algebraic, Rational, SafeLong}
import spire.syntax.cfor._
import spire.syntax.field._
// import spire.syntax.truncatedDivision._
import spire.syntax.eq._
import spire.util.Opt

final class RealCyclo protected[cyclo](val underlying: Cyclo) {

  override def hashCode: Int = underlying.hashCode

  protected def defaultPrint(sb: StringBuilder): Unit = {
    import underlying.{order, exps, coeffs, nTerms}

    def cosTerm(exp: Int): Unit = {
      require(exp != 0)
      sb ++= "cos("
      val n: Long = 2 * exp
      val d: Long = order
      val g = spire.math.gcd(n, d)
      val n1 = n / g
      val d1 = d / g
      if (n1 < 0)
        sb ++= "-"
      val n2 = spire.math.abs(n1)
      if (n2 == 1) {
        sb ++= "pi"
      } else {
        sb ++= n2.toString
        sb ++= "*pi"
      }
      if (d1 != 1) {
        sb ++= "/"
        sb ++= d1.toString
      }
      sb ++= ")"
    }

    def leadingTerm(exp: Int, coeff: Rational): Unit =
      if (exp == 0) sb ++= coeff.toString
      else if (coeff.isOne) cosTerm(exp)
      else if (coeff == -1) {
        sb ++= "-";
        cosTerm(exp)
      }
      else if (coeff > 0) {
        sb ++= coeff.toString;
        sb ++= "*";
        cosTerm(exp)
      }
      else if (coeff < 0) {
        sb ++= "-";
        sb ++= coeff.abs.toString;
        sb ++= "*";
        cosTerm(exp)
      }
      else sys.error("Coefficient coeff cannot be zero")

    leadingTerm(exps(0), coeffs(0))

    def followingTerm(exp: Int, coeff: Rational): Unit =
      if (coeff.isOne) {
        sb ++= " + ";
        cosTerm(exp)
      }
      else if (coeff == -1) {
        sb ++= " - ";
        cosTerm(exp)
      }
      else if (coeff > 0) {
        sb ++= " + ";
        sb ++= coeff.toString;
        sb ++= "*";
        cosTerm(exp)
      }
      else if (coeff < 0) {
        sb ++= " - ";
        sb ++= coeff.abs.toString;
        sb ++= "*";
        cosTerm(exp)
      }
      else sys.error("Coefficient coeff cannot be zero")

    cforRange(1 until nTerms) { k =>
      followingTerm(exps(k), coeffs(k))
    }
  }

  override def toString: String = {
    val sb = new StringBuilder
    Quadratic.fromCycloOpt(underlying) match {
      case Opt(q) => q.print(sb)
      case _ =>
        defaultPrint(sb)
    }
    sb.result()
  }

  override def equals(any: Any) = any match {
    case that: RealCyclo => this.underlying === that.underlying
    case that: Cyclo => this.underlying === that
  }

  def isZero: Boolean = underlying.isZero

  def isOne: Boolean = underlying.isOne

  def unary_- : RealCyclo = new RealCyclo(-underlying)

  def +(r: Rational): RealCyclo = new RealCyclo(underlying + r)
  def -(r: Rational): RealCyclo = new RealCyclo(underlying - r)
  def *(r: Rational): RealCyclo = new RealCyclo(underlying * r)
  def /(r: Rational): RealCyclo = new RealCyclo(underlying / r)

  def +(r: RealCyclo): RealCyclo = new RealCyclo(underlying + r.underlying)
  def -(r: RealCyclo): RealCyclo = new RealCyclo(underlying - r.underlying)
  def *(r: RealCyclo): RealCyclo = new RealCyclo(underlying * r.underlying)
  def /(r: RealCyclo): RealCyclo = new RealCyclo(underlying / r.underlying)

  def pow(rhs: Int): RealCyclo = new RealCyclo(underlying.pow(rhs))

  def reciprocal: RealCyclo = new RealCyclo((underlying.reciprocal))

  def isRational: Boolean = underlying.isRational

  def toRational: Rational = underlying.toRational

  lazy val toAlgebraic: Algebraic = {
    var sum = Algebraic.Zero
    cforRange(0 until underlying.nTerms) { i =>
      sum += Algebraic(underlying.coeffs(i)) * RealCyclo.cosRevAlgebraic(Rational(underlying.exps(i), underlying.order))
    }
    sum
  }

}


trait RealCycloSigned extends SignedAdditiveAbGroup[RealCyclo] /* with TruncatedDivisionCRing[RealCyclo] */ {

/*
  def toBigIntOpt(x: RealCyclo): Opt[BigInt] =
    if (!x.underlying.isRational) Opt.empty[BigInt]
    else x.underlying.toRational.toBigIntOpt

  def tquot(x: RealCyclo, y: RealCyclo): RealCyclo = RealCyclo((x / y).toAlgebraic.toBigDecimal(0, RoundingMode.DOWN).toBigInt)

  def tmod(x: RealCyclo, y: RealCyclo): RealCyclo = x - tquot(x, y) * y
 */
  def compare(x: RealCyclo, y: RealCyclo): Int = Order[Algebraic].compare(x.toAlgebraic, y.toAlgebraic)

}

trait RealCycloField extends Field.WithDefaultGCD[RealCyclo] {
  def plus(c1: RealCyclo, c2: RealCyclo) = new RealCyclo(c1.underlying + c2.underlying)
  override def minus(c1: RealCyclo, c2: RealCyclo) = new RealCyclo(c1.underlying - c2.underlying)
  def times(c1: RealCyclo, c2: RealCyclo) = new RealCyclo(c1.underlying * c2.underlying)
  def negate(c: RealCyclo) = new RealCyclo(-c.underlying)
  def zero = RealCyclo.zero
  override def fromInt(n: Int) =
    if (n == 0) RealCyclo.zero else RealCyclo(n)

  def one = RealCyclo.one
  override def reciprocal(c: RealCyclo) = new RealCyclo(c.underlying.reciprocal)
  def div(c1: RealCyclo, c2: RealCyclo) = new RealCyclo(c1.underlying / c2.underlying)


  override def pow(a: RealCyclo, n: Int): RealCyclo = new RealCyclo(a.underlying.pow(n))
  override def isZero(a: RealCyclo)(implicit ev: Eq[RealCyclo]) = a.underlying.isZero

  override def isOne(a: RealCyclo)(implicit ev: Eq[RealCyclo]) = a.underlying.isOne
}

object RealCyclo {

  final val maxOrder = 1024

  protected val firstKind = spire.math.poly.SpecialPolynomials.chebyshevsFirstKind[Rational](maxOrder)
  protected val secondKind = spire.math.poly.SpecialPolynomials.chebyshevsSecondKind[Rational](maxOrder)

  implicit object algebra extends RealCycloSigned with RealCycloField

  val zero = new RealCyclo(Cyclo.zero)

  val one = new RealCyclo(Cyclo.one)

  def apply(i: Int): RealCyclo = new RealCyclo(Cyclo(i))

  def apply(r: Rational): RealCyclo = new RealCyclo(Cyclo(r))

  def real(c: Cyclo): RealCyclo = new RealCyclo(c.real)

  def imag(c: Cyclo): RealCyclo = new RealCyclo(c.imag)

  val sqrt2: RealCyclo = new RealCyclo(Cyclo.sqrt2)

  def sqrt(r: Rational): RealCyclo =
    if (r < 0) sys.error("Only square root of nonnegative numbers are real")
    else new RealCyclo(Cyclo.sqrt(r))

  def sqrt(n: Int): RealCyclo =
    if (n < 0) sys.error("Only square root of nonnegative numbers are real")
    else new RealCyclo(Cyclo.sqrt(n))

  def sinDeg(d: Rational): RealCyclo = new RealCyclo(Cyclo.sinDeg(d))

  def cosDeg(d: Rational): RealCyclo = new RealCyclo(Cyclo.cosDeg(d))

  def sinRev(n: Rational): RealCyclo = new RealCyclo(Cyclo.sinRev(n))

  def cosRev(n: Rational): RealCyclo = new RealCyclo(Cyclo.cosRev(n))

  def cosFracPiAlgebraic(num: SafeLong, den: SafeLong): Algebraic = {
    assert(num.isValidInt && den.isValidInt)
    val n = num.toInt
    val d = den.toInt
    if (n.isOdd && d.isEven) {
      // can use Chebyshev first kind
      // Cheb first kind has roots cos( (2k-1)/(2m) * pi )
      val k = (n + 1) / 2
      val m = (d / 2)
      val poly = firstKind(m)
      Algebraic.root(poly, m - k)
    } else {
      // Cheb second kind has root cos( k/(m+1) * pi )
      val k = n
      val m = (d - 1)
      val poly = secondKind(m)
      Algebraic.root(poly, m - k)
    }
  }

  def fmod(lhs: SafeLong, rhs: SafeLong): SafeLong = {
    val tm = lhs % rhs
    if (tm.signum == -rhs.signum) tm + rhs else tm
  } 

  def cosRevAlgebraic(nRev: Rational): Algebraic = {
    val d: SafeLong = nRev.denominator
    val n: SafeLong = fmod(nRev.numerator, d) // TODO: replace by truncated division operator
    if (n.isZero) return Algebraic.One
    if (2 * n === d) return -Algebraic.One
    if (d.isOdd) {
      if (2 * n > d)
        cosFracPiAlgebraic(2 * (d - n), d)
      else
        cosFracPiAlgebraic(2 * n, d)
    } else {
      if (2 * n > d)
        cosFracPiAlgebraic(d - n, d / 2)
      else
        cosFracPiAlgebraic(n, d / 2)
    }
  }

}
