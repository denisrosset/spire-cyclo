package cyclo

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.generic.CanBuildFrom
import scala.util.{Try, Success, Failure}

import spire.algebra._
import spire.compat._
import spire.math.{Complex, Rational, SafeLong}
import spire.std.int._
import spire.std.double._
import spire.std.map._
import spire.syntax.all._
import spire.util.Opt

class Cyclo(val order: Int, protected[cyclo] val exps: Array[Int], protected[cyclo] val values: Array[Rational]) { lhs =>
/*
  def multiplyExponents(j: Int): Cyclo =
    if (!spire.math.gcd(j, order).isOne)
      sys.error("multiplyExponents needs gcd == 1")
    else
      Reduction.mkCyclotomic(order, coeffs.mapKeys(k => (j * k) % order))

  def numberOfCoeffs: Int = coeffs.size // TODO: remove

  override def toString: String = {

    def showBaseExp(ex: Int): String =
      if (ex.isOne) s"e($order)" else s"e($order)^$ex"

    def leadingTerm(r: Rational, ex: Int): String =
      if (ex.isZero) r.toString else {
        val t = showBaseExp(ex)
        if (r.isOne) t
        else if (r == -1) "-" + t
        else if (r > 0) r.toString + "*" + t
        else if (r < 0) "-" + r.abs.toString + "*" + t
        else ""
      }

    def followingTerms(list: List[(Int, Rational)]): String = list match {
      case Nil => ""
      case (ex, rat) :: xs => followingTerm(rat, ex) + followingTerms(xs)
    }

    def followingTerm(r: Rational, ex: Int): String = {
      val t = showBaseExp(ex)
      if (r.isOne) s" + $t"
      else if (r == -1) s" - $t"
      else if (r > 0) " + " + r.toString + "*" + t
      else if (r < 0) " - " + r.abs.toString + "*" + t
      else ""
    }

    coeffs.toList match {
      case Nil => "0"
      case ((ex, rat) :: xs) => leadingTerm(rat, ex) + followingTerms(xs)
    }

  }

  def *(r: Rational): Cyclo =
    if (r.isZero) Cyclo.zero else Cyclo(order, coeffs.mapValues(r * _ ))

  def *(rhs: Cyclo): Cyclo = {
    val Cyclo(o1, map1) = lhs
    val Cyclo(o2, map2) = rhs
    val ord = spire.math.lcm(o1, o2)
    val m1 = ord / o1
    val m2 = ord / o2
    // TODO: optimize
    var mp = SortedMap.empty[Int, Rational]
    for( (e1, c1) <- map1; (e2, c2) <- map2 ) {
      val k = (m1 * e1 + m2 * e2) % ord
      val v = c1 * c2
      mp = mp.insertWith(k, v, _ + _)
    }
    Reduction.mkCyclotomic(ord, mp)
  }

  def /(rhs: Cyclo): Cyclo = this * rhs.reciprocal

  def +(r: Rational): Cyclo =
    if (r.isZero) this else Cyclo(order, coeffs.insertWith(0, r, _ + _))

  def +(rhs: Cyclo): Cyclo = {
    val Cyclo(o1, map1) = lhs
    val Cyclo(o2, map2) = rhs
    val ord = spire.math.lcm(o1, o2)
    val m1 = ord / o1
    val m2 = ord / o2
    val map1p = map1.mapKeys( k => m1 * k )
    val map2p = map2.mapKeys( k => m2 * k )
    val newMap = map1p.unionWith(map2p, _ + _)
    Reduction.mkCyclotomic(ord, map1p.unionWith(map2p, _ + _))
  }

  def isReal: Boolean = this === conj // TODO

  def isRat: Boolean = order.isOne

  def isGaussianRat: Boolean = real.isRat && imag.isRat // TODO

  // TODO: rename to toDouble
  def toDouble: Option[Double] =
    if (isReal) Some(toComplex.real) else None

  def toComplex: Complex[Double] =
    coeffs.foldLeft(Complex.zero[Double]) {
      case (sum, (exp, r)) => sum + Complex.rootOfUnity[Double](order.toInt, exp.toInt) * Complex(r.toDouble) // TODO remove toInt
    }

  def toRat: Option[Rational] =
    if (order.isOne)
      Some(coeffs.getOrElse(0, Rational.zero))
    else
      None

  def reciprocal: Cyclo = {
    val prod = Cyclo.productOfGaloisConjugates(this)
    (this * prod).toRat match {
      case Some(r) => prod * r.inverse
      case None => sys.error("invCyc:  product of Galois conjugates not rational; this is a bug, please inform package maintainer")
    }
  }

  def **(pow: Int): Cyclo = // TODO: repeated squaring
      if (pow == 0) Cyclo.zero
      else if (pow == 1) this
      else if (pow < 0) (this ** (-pow)).reciprocal
      else this * (this ** (pow - 1))

  def conj: Cyclo = Reduction.mkCyclotomic(order, coeffs.mapKeys(k => (order - k) % order))

  def real: Cyclo = (this + conj) * Rational(1, 2)

  def imag: Cyclo = (this - conj) / (2*Cyclo.i)
 */
}
/*
trait CycloEq extends Eq[Cyclo] {

  def eqv(x: Cyclo, y: Cyclo) =
    (x.order === y.order) &&
    ((x.coeffs: Map[Int, Rational]) === (y.coeffs: Map[Int, Rational]))

}

trait CycloField extends Field[Cyclo] {

  def plus(c1: Cyclo, c2: Cyclo) = c1 + c2

  override def minus(c1: Cyclo, c2: Cyclo) = plus(c1, negate(c2)) // TODO: faster ?

  def times(c1: Cyclo, c2: Cyclo) = c1 * c2

  def negate(c: Cyclo) = c * Rational(-1)

  def zero = Cyclo.zero

  def abs(c: Cyclo) = Cyclo.absVal(c)
  // TODO: check interface
  override def fromInt(n: Int) =
    if (n == 0) Cyclo.zero else Cyclo(1, SortedMap(Int(0) -> Rational(n)))

  override def reciprocal(c: Cyclo) = c.reciprocal

  def div(c1: Cyclo, c2: Cyclo) = c1 / c2

  def one = Cyclo(1, SortedMap(Int(0) -> Rational.one))

  def gcd(c1: Cyclo, c2: Cyclo): Cyclo = ???
  def mod(c1: Cyclo, c2: Cyclo): Cyclo = ???
  def quot(c1: Cyclo, c2: Cyclo): Cyclo = ???

}

object Cyclo {

  import Reduction._

  val zero = Cyclo(1, Coeffs.empty)

  implicit def fromRational(r: Rational): Cyclo =
    if (r.isZero) Cyclo.zero else Cyclo(1, Coeffs(Int(0) -> r))

  val one = fromRational(Rational.one)

  implicit val field: Field[Cyclo] = new CycloField { }

  implicit val equ: Eq[Cyclo] = new CycloEq { }

  def e(n: Int): Cyclo =
    if (n < 1) sys.error("e requires a positive integer")
    else if (n == 1) Cyclo.one
    else cyclotomic(n, convertToBase(n, SortedMap(Int(1) -> Rational.one)))

  def eb(n: Int): Cyclo =
    if (n < 1) sys.error("eb needs a positive integer")
    else if (n % 2 != 1) sys.error("eb needs an odd integer")
    else if (n == 1) zero
    else {
      val en = e(n)
      @tailrec def loop(k: Int, sum: Cyclo): Cyclo =
        if (k > (n - 1) / 2) sum
        else loop(k + 1, sum + en ** ((k * k) % n))
      loop(1, Cyclo.zero)
    }

  def sqrt2: Cyclo = e(8) - e(8)**3

  def factorise(n: Int): Map[Int, Int] = spire.math.prime.Factors(n).toMap

  def sqrtInt(n: Int): Cyclo =
    if (n.signum == 0) zero
    else if (n.signum < 0) i * sqrtPositiveInt(-n)
    else sqrtPositiveInt(n)

  def sqrtPositiveInt(n: Int): Cyclo =
    if (n < 1) sys.error("sqrtPositiveInt needs a positive integer")
    else {
      val factors = Factors(n.toInt) // TODO replace
      val factor = factors.squarePartSqrt
      val squareFreePart = factors.squareFreePart
      (squareFreePart % 4).toInt match {
        case 1 => (eb(squareFreePart) * 2 + 1) * factor
        case 2 => (sqrt2 * sqrtPositiveInt(squareFreePart / 2)) * factor
        case 3 => -i * (eb(squareFreePart) * 2 + 1) * factor
        case _ => sqrtPositiveInt(squareFreePart / 4) * (factor * 2)
      }
    }

  def sqrtRat(r: Rational): Cyclo =
    sqrtInt(r.numerator * r.denominator) * Rational(1, r.denominator)

  val i: Cyclo = e(4)

  def gaussianRat(p: Rational, q: Rational): Cyclo =
    fromRational(p)  + fromRational(q) * i

  def polarRat(r: Rational, s: Rational): Cyclo = {
    val p = s.numerator
    val q = s.denominator
    if (p.signum >= 0)
      fromRational(r) * (e(q)**p)
    else
      (fromRational(r) * (e(q)**(-p))).conj
  }

  def polarRatDeg(r: Rational, deg: Rational): Cyclo = polarRat(r, deg / 360)

  def absVal(c: Cyclo): Try[Cyclo] = {
    val modsq = c * c.conj
    modsq.toRat match {
      case Some(msq) => Success(sqrtRat(msq))
      case None => Try(sys.error("abs not available for this number"))
    }
  }

  // gives the normalized complex number with norm = 1
  def sigNum(c: Cyclo): Cyclo =
    if (c.isZero) zero else absVal(c).map( a => c / a).get // TODO: throws


  def productOfGaloisConjugates(c: Cyclo): Cyclo = {
    @tailrec def loop(j: Int, prd: Cyclo): Cyclo =
      if (j == c.order) prd
      else if (spire.math.gcd(c.order.toInt, j) == 1) loop(j + 1, prd * c.multiplyExponents(j)) // TODO
      else loop(j + 1, prd)
    loop(2, Cyclo.one)
  }

  def sinDeg(d: Rational): Cyclo = sinRev(d/ 360)

  def cosDeg(d: Rational): Cyclo = cosRev(d / 360)

  def sinRev(n: Rational): Cyclo = {
    val nm = n.numerator.abs
    val dn = n.denominator
    val a = e(dn) ** nm
    fromRational(dn.signum) * (a - a.conj) / (2*i)
  }

  def cosRev(n: Rational): Cyclo = {
    val nm = n.numerator.abs
    val dn = n.denominator
    val a = e(dn) ** nm
    (a + a.conj) / 2
  }

}
 */
