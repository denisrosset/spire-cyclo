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

case class Cyclo(order: SafeLong, coeffs: SortedMap[SafeLong, Rational]) {
  import Cyclo.{leadingTerm, followingTerms}

  override def toString: String = coeffs.toList match {
    case Nil => "0"
    case ((ex, rat) :: xs) => leadingTerm(rat, order, ex) + followingTerms(order, xs)
  }

}


trait CycloEq extends Eq[Cyclo] {

  def eqv(x: Cyclo, y: Cyclo) =
    (x.order === y.order) &&
    ((x.coeffs: Map[SafeLong, Rational]) === (y.coeffs: Map[SafeLong, Rational]))

}

trait CycloField extends Field[Cyclo] {

  def plus(c1: Cyclo, c2: Cyclo) = Cyclo.sumCyc(c1, c2)

  override def minus(c1: Cyclo, c2: Cyclo) = Cyclo.sumCyc(c1, Cyclo.aInvCyc(c2))

  def times(c1: Cyclo, c2: Cyclo) = Cyclo.prodCyc(c1, c2)

  def negate(c: Cyclo) = Cyclo.aInvCyc(c)

  def zero = Cyclo.zeroCyc

  def abs(c: Cyclo) = Cyclo.absVal(c)
  // TODO: check interface
  override def fromInt(n: Int) =
    if (n == 0) Cyclo.zeroCyc else Cyclo(1, SortedMap(SafeLong(0) -> Rational(n)))

  def inverse(c: Cyclo) = Cyclo.invCyc(c)

  def div(c1: Cyclo, c2: Cyclo) = times(c1, inverse(c2))

  def one = Cyclo(1, SortedMap(SafeLong(0) -> Rational.one))

  def gcd(c1: Cyclo, c2: Cyclo): Cyclo = ???
  def mod(c1: Cyclo, c2: Cyclo): Cyclo = ???
  def quot(c1: Cyclo, c2: Cyclo): Cyclo = ???

}

object Cyclo {

  implicit def fromRational(r: Rational): Cyclo =
    if (r.isZero) Cyclo.zeroCyc else Cyclo(1, SortedMap(SafeLong(0) -> Rational(r)))

  implicit object field extends CycloField

  implicit object equ extends CycloEq

  def e(n: SafeLong): Cyclo =
    if (n < 1) sys.error("e requires a positive integer")
    else if (n == 1) Cyclo(1, SortedMap(SafeLong(0) -> Rational.one))
    else cyclotomic(n, convertToBase(n, SortedMap(SafeLong(1) -> Rational.one)))

  def showBaseExp(n: SafeLong, ex: SafeLong): String =
    if (ex.isOne) s"e($n)" else s"e($n)^ex"

  def leadingTerm(r: Rational, n: SafeLong, ex: SafeLong): String =
    if (ex.isZero) showRat(r) else {
      val t = showBaseExp(n, ex)
      if (r.isOne) t
      else if (r == -1) "-" + t
      else if (r > 0) showRat(r) + "*" + t
      else if (r < 0) "-" + showRat(r.abs) + "*" + t
      else ""
    }

  def followingTerms(n: SafeLong, list: List[(SafeLong, Rational)]): String = list match {
    case Nil => ""
    case (ex, rat) :: xs => followingTerm(rat, n, ex) + followingTerms(n, xs)
  }

  def followingTerm(r: Rational, n: SafeLong, ex: SafeLong): String = {
    val t = showBaseExp(n, ex)
    if (r.isOne) s" + $t"
    else if (r == -1) s" - $t"
    else if (r > 0) " + " + showRat(r) + "*" + t
    else if (r < 0) " - " + showRat(r.abs) + "*" + t
    else ""
  }

  def showRat(r: Rational): String = {
    val n = r.numerator
    val d = r.denominator
    if (d == 1)
      s"$n"
    else
      s"$n/$d"
  }

  implicit class CycloIterable(cs: Traversable[Cyclo]) {

    def sumAll: Cyclo = if (cs.isEmpty) zeroCyc else cs.reduce(_ + _)

    def productAll: Cyclo = if (cs.isEmpty) fromRational(0) else cs.reduce(_ * _)

  }

  def eb(n: SafeLong): Cyclo =
    if (n < 1) sys.error("eb needs a positive integer")
    else if (n % 2 != 1) sys.error("eb needs an odd integer")
    else if (n == 1) zeroCyc
    else {
      val en = e(n)
      enumFromTo(1, (n - 1) / 2).map(k => en**((k*k) % n)).sumAll
    }

  // TODO: replace
  implicit class RichCyclo(c: Cyclo) {

    def **(pow: SafeLong): Cyclo =
      if (pow == 0) zeroCyc
      else if (pow == 1) c
      else if (pow < 0) invCyc(c ** (-pow))
      else c ** (pow - 1)

  }

  def sqrt2: Cyclo = e(8) - e(8)**3

  def factorise(n: SafeLong): Map[SafeLong, Int] = spire.math.prime.Factors(n).toMap

  def sqrtInteger(n: SafeLong): Cyclo =
    if (n.signum == 0) zeroCyc
    else if (n.signum < 0) i * sqrtPositiveInteger(-n)
    else sqrtPositiveInteger(n)

  def sqrtPositiveInteger(n: SafeLong): Cyclo =
    if (n < 1) sys.error("sqrtPositiveInteger needs a positive integer")
    else {
      val factors = factorise(n)
      val factor = factors.map { case (p, m) => p**(m / 2) }.product
      val nn = factors.map { case (p, m) => p**(m % 2) }.product
      (nn % 4).toInt match {
        case 1 => prodRatCyc(factor, sumRatCyc(1, prodRatCyc(2, eb(nn))))
        case 2 => prodRatCyc(factor, sqrt2 * sqrtPositiveInteger(nn / 2))
        case 3 => prodRatCyc(factor, (-i) * sumRatCyc(1, prodRatCyc(2, eb(nn))))
        case _ => prodRatCyc(factor * 2, sqrtPositiveInteger(nn / 4))
      }
    }

  def sqrtRat(r: Rational): Cyclo =
    prodRatCyc(Rational(1, r.denominator), sqrtInteger(r.numerator * r.denominator))

  val i: Cyclo = e(4)

  def gaussianRat(p: Rational, q: Rational): Cyclo =
    fromRational(p)  + fromRational(q) * i

  def polarRat(r: Rational, s: Rational): Cyclo = {
    val p = s.numerator
    val q = s.denominator
    if (p.signum >= 0)
      fromRational(r) * (e(q)**p)
    else
      conj(fromRational(r) * (e(q)**(-p)))
  }

  def polarRatDeg(r: Rational, deg: Rational): Cyclo = polarRat(r, deg / 360)

  def conj(c: Cyclo): Cyclo = {
    val Cyclo(n, mp) = c
    mkCyclotomic(n, mp.mapKeys(k => (n - k) % n))
  }

  def real(z: Cyclo): Cyclo = (z + conj(z)) / 2

  def imag(z: Cyclo): Cyclo = (z - conj(z)) / (2*i)

  def absVal(c: Cyclo): Try[Cyclo] = {
    val modsq = c * conj(c)
    toRat(modsq) match {
      case Some(msq) => Success(sqrtRat(msq))
      case None => Try(sys.error("abs not available for this number"))
    }
  }

  // gives the normalized complex number with norm = 1
  def sigNum(c: Cyclo): Cyclo =
    if (c.isZero) zeroCyc else absVal(c).map( a => c / a).get // TODO: throws

  def convertToBase(n: SafeLong, mp: SortedMap[SafeLong, Rational]): SortedMap[SafeLong, Rational] =
    extraneousPowers(n).foldRight(mp) {
      case ((p, r), m) => replace(n, p, r, m)
    }

  def removeZeros(mp: SortedMap[SafeLong, Rational]): SortedMap[SafeLong, Rational] =
    mp.filterNot { case (k, v) => v.isZero }

  // Corresponds to GAP implementation.
  // Expects that convertToBase has already been done.

  def cyclotomic(ord: SafeLong, mp: SortedMap[SafeLong, Rational]): Cyclo =
    tryReduce(tryRational(gcdReduce(Cyclo(ord, mp))))

  def mkCyclotomic(ord: SafeLong, mp: SortedMap[SafeLong, Rational]): Cyclo =
    cyclotomic(ord, removeZeros(convertToBase(ord, mp)))

  // Step 1 of cyclotomic is gcd reduction.
  def gcdReduce(cyc: Cyclo): Cyclo = {
    val Cyclo(n, mp) = cyc
    val d = gcdCyc(cyc)
    if (d.isOne) cyc else Cyclo(n / d, mp.mapKeys(_ / d))
  }

  def gcdCyc(cyc: Cyclo): SafeLong = gcdList(cyc.order :: cyc.coeffs.keys.toList)

  def gcdList(list: List[SafeLong]): SafeLong =
    if (list.isEmpty)
      sys.error("gcdList called on empty list")
    else
      list.reduce(spire.math.gcd(_,_))

  // Step 2 of cyclotomic is reduction to a rational if possible.
  def tryRational(c: Cyclo): Cyclo = {
    val (phi, nrp, sqfree) = phiNrpSqfree(c.order)

    if (SafeLong(lenCyc(c)) === phi && sqfree)
      equalCoefficients(c) match {
        case None => c
        case Some(r) => fromRational( ((-1)**(nrp % 2))*r )
      }
      else c
  }

  // Compute phi(n), the number of prime factors, and test if n is square-free.
  // We do these all together for efficiency, so we only call factorise once.

  def phiNrpSqfree(n: SafeLong): (SafeLong, Int, Boolean) = {
    val factors = factorise(n)
    val phi = factors.map(_._1).foldRight(n) {
      case (p, nprime) => (nprime / p) * (p - 1)
    }
    val nrp = factors.size
    val sqfree = factors.forall(_._2 <= 1)
    (phi, nrp, sqfree)
  }

  def equalCoefficients(cyc: Cyclo): Option[Rational] =
    cyc.coeffs.values.toList match {
      case Nil => None
      case hd :: tl if tl.forall(_ == hd) => Some(hd)
    }

  def lenCyc(cyc: Cyclo): Int = removeZeros(cyc.coeffs).size

  // Step 3 of cyclotomic is base reduction
  def tryReduce(c: Cyclo): Cyclo = {
    val squareFreeOddFactors =
      factorise(c.order).filter { case (p, m) => p > 2 && m <= 1 }.map(_._1)
    squareFreeOddFactors.foldRight(c) {
      case (prime, c1) => reduceByPrime(prime, c1)
    }
  }


  case class Increasing(fst: SafeLong, step: SafeLong, lst: SafeLong) extends IndexedSeq[SafeLong] {
    val sfLength = (lst - fst + step)/step
    require(sfLength.isValidInt)
    val length: Int = sfLength.toInt
    def apply(k: Int): SafeLong = fst + step * k
  }

  case class Decreasing(fst: SafeLong, step: SafeLong, lst: SafeLong) extends IndexedSeq[SafeLong] {
    val sfLength = (lst - fst + step)/step
    require(sfLength.isValidInt)
    val length: Int = sfLength.toInt
    def apply(k: Int): SafeLong = fst + step * k
  }

  def enumFromTo(first: SafeLong, last: SafeLong): IndexedSeq[SafeLong] =
    if (first == last) IndexedSeq(first)
    else if (first < last) Increasing(first, first + 1, last)
    else Decreasing(first, first - 1, last)

  def enumFromThenTo(first: SafeLong, second: SafeLong, last: SafeLong): IndexedSeq[SafeLong] =
    if (first == second) sys.error("First element is equal to second element")
    else if (first < second && first <= last) Increasing(first, second - first, last)
    else if (first > second && first >= last) Decreasing(first, second - first, last)
    else IndexedSeq.empty[SafeLong]

  def reduceByPrime(p: SafeLong, c: Cyclo): Cyclo = {
    val n = c.order
    val mb = SortedMap.newBuilder[SafeLong, Rational]
    for( (r, k) <- enumFromThenTo(0, p, n - p).zipWithIndex ) {
      equalReplacements(p, r, c) match {
        case Some(rep) => mb += (SafeLong(k) -> rep)
        case None => return c
      }
    }
    Cyclo(n / p, removeZeros(mb.result))
  }

  def equalReplacements(p: SafeLong, r: SafeLong, c: Cyclo): Option[Rational] = c match {
    case Cyclo(n, mp) =>
      replacements(n, p, r).map( k => mp.getOrElse(k, Rational.zero) ) match {
        case Nil => sys.error("equalReplacements generated empty list")
        case x :: xs if xs.forall(_ === x) => Some(x)
        case _ => None
      }
  }

  def replacements(n: SafeLong, p: SafeLong, r: SafeLong): List[SafeLong] = {
    val s = n / p
    enumFromThenTo(r-s, r-2*s, 0).toList ::: enumFromThenTo(r+s, r+2*s, n - 1).toList
  }

  def replace(n: SafeLong, p: SafeLong, r: SafeLong, mp: SortedMap[SafeLong, Rational]): SortedMap[SafeLong, Rational] = mp.get(r) match {
    case None => mp
    case Some(rat) => replacements(n, p, r).foldRight(mp - r) {
      case (k, m) => m.insertWith(k, -rat, _ + _)
    }
  }

  def includeMods(n: SafeLong, q: SafeLong, start: SafeLong): List[SafeLong] =
    List(start) ++ enumFromThenTo(start - q, start - 2 * q, 0) ++ enumFromThenTo(start + q, start + 2 * q, n - 1)

  def removeExps(n: SafeLong, p: SafeLong, q: SafeLong): List[SafeLong] = {
    val ndq = n / q
    if (p == 2) enumFromTo(q / 2, q - 1).map(_ * ndq).toList.flatMap(includeMods(n, q, _))
    else {
      val m = (q / p - 1) / 2
      enumFromTo(-m, m).map(_ * ndq).toList.flatMap(includeMods(n, q, _))
    }
  }

  def pqPairs(n: SafeLong): List[(SafeLong, SafeLong)] =
    factorise(n).toList.map { case (p, k) => (p, p ** k) }
  
  def extraneousPowers(n: SafeLong): List[(SafeLong, SafeLong)] =
    if (n < 1) sys.error("extraneousPowers needs a positive integer")
    else
      pqPairs(n)
        .flatMap { case (p, q) => removeExps(n, p, q).map( r => (p, r) ) }
        .distinct

  def sumCyc(c1: Cyclo, c2: Cyclo): Cyclo = {
    val Cyclo(o1, map1) = c1
    val Cyclo(o2, map2) = c2
    val ord = spire.math.lcm(o1, o2)
    val m1 = ord / o1
    val m2 = ord / o2
    val map1p = map1.mapKeys( k => m1 * k )
    val map2p = map2.mapKeys( k => m2 * k )
    mkCyclotomic(ord, map1p.unionWith(map2p, _ + _))
  }

  def prodCyc(c1: Cyclo, c2: Cyclo): Cyclo = {
    val Cyclo(o1, map1) = c1
    val Cyclo(o2, map2) = c2
    val ord = spire.math.lcm(o1, o2)
    val m1 = ord / o1
    val m2 = ord / o2
    // TODO: optimize
    var mp = SortedMap.empty[SafeLong, Rational]
    for( (e1, c1) <- map1; (e2, c2) <- map2 ) {
      val k = (m1 * e1 + m2 * e2) % ord
      val v = c1 * c2
      mp = mp.insertWith(k, v, _ + _)
    }
    Cyclo(ord, mp)
  }

  def prodRatCyc(r: Rational, c: Cyclo): Cyclo =
    if (r.isZero)
      zeroCyc
    else
      Cyclo(c.order, c.coeffs.mapValues(r * _ ))

  def sumRatCyc(r: Rational, c: Cyclo): Cyclo =
    Cyclo(c.order, c.coeffs.insertWith(SafeLong(0), r, _ + _))

  def zeroCyc = Cyclo(1, SortedMap.empty[SafeLong, Rational])

  def aInvCyc(c: Cyclo): Cyclo = prodRatCyc(-1, c)

  def multiplyExponents(j: SafeLong, c: Cyclo): Cyclo = {
    val Cyclo(n, mp) = c
    if (!spire.math.gcd(j, n).isOne)
      sys.error("multiplyExponents needs gcd == 1")
    else
      mkCyclotomic(n, mp.mapKeys(k => (j * k) % n))
  }

  def productOfGaloisConjugates(c: Cyclo): Cyclo = {
    enumFromTo(2, c.order)
      .filter( j => j.gcd(c.order).isOne )
      .map( j => multiplyExponents(j, c) )
      .productAll
  }

  def invCyc(z: Cyclo): Cyclo = {
    val prod = productOfGaloisConjugates(z)
    toRat(z * prod) match {
      case Some(r) => prodRatCyc(r.inverse, prod)
      case None => sys.error("invCyc:  product of Galois conjugates not rational; this is a bug, please inform package maintainer")
    }
  }

  def isReal(c: Cyclo): Boolean = c === conj(c)

  def isRat(c: Cyclo): Boolean = c.order.isOne

  def isGaussianRat(c: Cyclo): Boolean = isRat(real(c)) && isRat(imag(c))

  def toComplex(c: Cyclo): Complex[Double] = ??? // TODO: implement

  // TODO: rename to toDouble
  def toReal(c: Cyclo): Option[Double] =
    if (isReal(c)) Some(toComplex(c).real) else None

  def toRat(c: Cyclo): Option[Rational] =
    if (c.order.isOne)
      Some(c.coeffs.getOrElse(0, Rational.zero))
    else
      None

  def sinDeg(d: Rational): Cyclo = sinRev(d/ 360)

  def cosDeg(d: Rational): Cyclo = cosRev(d / 360)

  def sinRev(n: Rational): Cyclo = {
    val nm = n.numerator.abs
    val dn = n.denominator
    val a = e(dn) ** nm
    fromRational(dn.signum) * (a - conj(a)) / (2*i)
  }

  def cosRev(n: Rational): Cyclo = {
    val nm = n.numerator.abs
    val dn = n.denominator
    val a = e(dn) ** nm
    (a + conj(a)) / 2
  }

}
