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

object Reduction {

  import Cyclo._
  // Corresponds to GAP implementation.
  // Expects that convertToBase has already been done.

  def cyclotomic(ord: Integer, mp: SortedMap[Integer, Rational]): Cyclo =
    tryReduce(tryRational(gcdReduce(Cyclo(ord, mp))))

  def mkCyclotomic(ord: Integer, mp: SortedMap[Integer, Rational]): Cyclo =
    cyclotomic(ord, removeZeros(convertToBase(ord, mp)))
  // Step 1 of cyclotomic is gcd reduction.
  def gcdReduce(cyc: Cyclo): Cyclo = {
    val Cyclo(n, mp) = cyc
    val d = gcdCyc(cyc)
    if (d.isOne) cyc else Cyclo(n / d, mp.mapKeys(_ / d))
  }

  def gcdCyc(cyc: Cyclo): Integer = gcdList(cyc.order :: cyc.coeffs.keys.toList)

  def gcdList(list: List[SafeLong]): Integer =
    if (list.isEmpty)
      sys.error("gcdList called on empty list")
    else
      list.reduce(spire.math.gcd(_,_))

  // Step 2 of cyclotomic is reduction to a rational if possible.
  def tryRational(c: Cyclo): Cyclo = {
    val (phi, nrp, sqfree) = phiNrpSqfree(c.order)

    if (Integer(lenCyc(c)) === phi && sqfree)
      equalCoefficients(c) match {
        case None => c
        case Some(r) => fromRational( ((-1)**(nrp % 2))*r )
      }
      else c
  }

  // Compute phi(n), the number of prime factors, and test if n is square-free.
  // We do these all together for efficiency, so we only call factorise once.
  def phiNrpSqfree(n: Integer): (Integer, Int, Boolean) = {
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

  case class Increasing(fst: Integer, step: Integer, lst: Integer) extends IndexedSeq[Integer] {
    val sfLength = (lst - fst + step)/step
    require(sfLength.isValidInt)
    val length: Int = sfLength.toInt
    def apply(k: Int): Integer = fst + step * k
  }

  case class Decreasing(fst: Integer, step: Integer, lst: Integer) extends IndexedSeq[Integer] {
    val sfLength = (lst - fst + step)/step
    require(sfLength.isValidInt)
    val length: Int = sfLength.toInt
    def apply(k: Int): Integer = fst + step * k
  }

  def enumFromTo(first: Integer, last: Integer): IndexedSeq[Integer] =
    if (first == last) IndexedSeq(first)
    else if (first < last) Increasing(first, first + 1, last)
    else Decreasing(first, first - 1, last)

  def enumFromThenTo(first: Integer, second: Integer, last: Integer): IndexedSeq[Integer] =
    if (first == second) sys.error("First element is equal to second element")
    else if (first < second && first <= last) Increasing(first, second - first, last)
    else if (first > second && first >= last) Decreasing(first, second - first, last)
    else IndexedSeq.empty[Integer]

  def reduceByPrime(p: Integer, c: Cyclo): Cyclo = {
    val n = c.order
    val mb = SortedMap.newBuilder[Integer, Rational]
    for( (r, k) <- enumFromThenTo(0, p, n - p).zipWithIndex ) {
      equalReplacements(p, r, c) match {
        case Some(rep) => mb += (Integer(k) -> rep)
        case None => return c
      }
    }
    Cyclo(n / p, removeZeros(mb.result))
  }

  def equalReplacements(p: Integer, r: Integer, c: Cyclo): Option[Rational] = c match {
    case Cyclo(n, mp) =>
      replacements(n, p, r).map( k => mp.getOrElse(k, Rational.zero) ) match {
        case Nil => sys.error("equalReplacements generated empty list")
        case x :: xs if xs.forall(_ === x) => Some(x)
        case _ => None
      }
  }

  def replacements(n: Integer, p: Integer, r: Integer): List[SafeLong] = {
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

  def multiplyExponents(j: SafeLong, c: Cyclo): Cyclo = {
    val Cyclo(n, mp) = c
    if (!spire.math.gcd(j, n).isOne)
      sys.error("multiplyExponents needs gcd == 1")
    else
      mkCyclotomic(n, mp.mapKeys(k => (j * k) % n))
  }

}
