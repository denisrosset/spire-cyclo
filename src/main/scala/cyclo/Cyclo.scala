package cyclo

import scala.collection.immutable.SortedMap
import scala.collection.generic.CanBuildFrom
import scala.util.{Try, Success, Failure}

import spire.algebra._
import spire.compat._
import spire.math.{Complex, Rational, SafeLong}
import spire.std.int._
import spire.std.map._
import spire.syntax.all._

case class Cyclo(order: Integer, coeffs: SortedMap[Integer, Rational]) { lhs =>

  override def toString: String = {

    def showBaseExp(ex: Integer): String =
      if (ex.isOne) s"e($order)" else s"e($order)^$ex"

    def leadingTerm(r: Rational, ex: SafeLong): String =
      if (ex.isZero) r.toString else {
        val t = showBaseExp(ex)
        if (r.isOne) t
        else if (r == -1) "-" + t
        else if (r > 0) r.toString + "*" + t
        else if (r < 0) "-" + r.abs.toString + "*" + t
        else ""
      }

    def followingTerms(list: List[(Integer, Rational)]): String = list match {
      case Nil => ""
      case (ex, rat) :: xs => followingTerm(rat, ex) + followingTerms(xs)
    }

    def followingTerm(r: Rational, ex: Integer): String = {
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
    var mp = SortedMap.empty[Integer, Rational]
    for( (e1, c1) <- map1; (e2, c2) <- map2 ) {
      val k = (m1 * e1 + m2 * e2) % ord
      val v = c1 * c2
      mp = mp.insertWith(k, v, _ + _)
    }
    Cyclo(ord, mp)
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
  def toReal: Option[Double] =
    if (isReal) Some(toComplex.real) else None

  def toComplex: Complex[Double] = ??? // TODO: implement

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

  def **(pow: Integer): Cyclo = // TODO: repeated squaring
      if (pow == 0) Cyclo.zero
      else if (pow == 1) this
      else if (pow < 0) (this ** (-pow)).reciprocal
      else this ** (pow - 1)

  def conj: Cyclo = Reduction.mkCyclotomic(order, coeffs.mapKeys(k => (order - k) % order))

  def real: Cyclo = (this + conj) / 2

  def imag: Cyclo = (this - conj) / (2*Cyclo.i)

}

trait CycloEq extends Eq[Cyclo] {

  def eqv(x: Cyclo, y: Cyclo) =
    (x.order === y.order) &&
    ((x.coeffs: Map[Integer, Rational]) === (y.coeffs: Map[Integer, Rational]))

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
    if (n == 0) Cyclo.zero else Cyclo(1, SortedMap(Integer(0) -> Rational(n)))

  override def reciprocal(c: Cyclo) = c.reciprocal

  def div(c1: Cyclo, c2: Cyclo) = c1 / c2

  def one = Cyclo(1, SortedMap(Integer(0) -> Rational.one))

  def gcd(c1: Cyclo, c2: Cyclo): Cyclo = ???
  def mod(c1: Cyclo, c2: Cyclo): Cyclo = ???
  def quot(c1: Cyclo, c2: Cyclo): Cyclo = ???

}

object Cyclo {

  import Reduction._

  val zero = Cyclo(1, Coeffs.empty)

  implicit def fromRational(r: Rational): Cyclo =
    if (r.isZero) Cyclo.zero else Cyclo(1, Coeffs(Integer(0) -> r))

  val one = fromRational(Rational.one)

  implicit val field: Field[Cyclo] = new CycloField { }

  implicit val equ: Eq[Cyclo] = new CycloEq { }

  def e(n: Integer): Cyclo =
    if (n < 1) sys.error("e requires a positive integer")
    else if (n == 1) Cyclo.one
    else cyclotomic(n, convertToBase(n, SortedMap(Integer(1) -> Rational.one)))

  implicit class CycloIterable(cs: Traversable[Cyclo]) {

    def sumAll: Cyclo = if (cs.isEmpty) zero else cs.reduce(_ + _)

    def productAll: Cyclo = if (cs.isEmpty) fromRational(0) else cs.reduce(_ * _)

  }

  def eb(n: Integer): Cyclo =
    if (n < 1) sys.error("eb needs a positive integer")
    else if (n % 2 != 1) sys.error("eb needs an odd integer")
    else if (n == 1) zero
    else {
      val en = e(n)
      enumFromTo(1, (n - 1) / 2).map(k => en**((k*k) % n)).sumAll
    }

  def sqrt2: Cyclo = e(8) - e(8)**3

  def factorise(n: Integer): Map[Integer, Int] = spire.math.prime.Factors(n).toMap

  def sqrtInteger(n: Integer): Cyclo =
    if (n.signum == 0) zero
    else if (n.signum < 0) i * sqrtPositiveInteger(-n)
    else sqrtPositiveInteger(n)

  def sqrtPositiveInteger(n: Integer): Cyclo =
    if (n < 1) sys.error("sqrtPositiveInteger needs a positive integer")
    else {
      val factors = factorise(n)
      val factor = factors.map { case (p, m) => p**(m / 2) }.product
      val nn = factors.map { case (p, m) => p**(m % 2) }.product
      (nn % 4).toInt match {
        case 1 => (eb(nn) * 2 + 1) * factor
        case 2 => (sqrt2 * sqrtPositiveInteger(nn / 2)) * factor
        case 3 => -i * (eb(nn) * 2 + 1) * factor
        case _ => sqrtPositiveInteger(nn / 4) * (factor * 2)
      }
    }

  def sqrtRat(r: Rational): Cyclo =
    sqrtInteger(r.numerator * r.denominator) * Rational(1, r.denominator)

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

  def convertToBase(n: Integer, mp: SortedMap[Integer, Rational]): SortedMap[Integer, Rational] =
    extraneousPowers(n).foldRight(mp) {
      case ((p, r), m) => replace(n, p, r, m)
    }

  def removeZeros(mp: SortedMap[Integer, Rational]): SortedMap[Integer, Rational] =
    mp.filterNot { case (k, v) => v.isZero }

  def productOfGaloisConjugates(c: Cyclo): Cyclo = {
    enumFromTo(2, c.order)
      .filter( j => j.gcd(c.order).isOne )
      .map( j => multiplyExponents(j, c) )
      .productAll
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
