package cyclo

import scala.annotation.tailrec

import spire.algebra._
import spire.math.{Rational, SafeLong}
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.util.Opt

final class Cyclo(val order: Int, // order of the cyclotomic
                  protected[cyclo] val exps: Array[Int], // exponents of the cyclotomic, in 0 .. order -1, increasing
                  protected[cyclo] val coeffs: Array[Rational] // corresponding (non-zero) values
                 ) { lhs =>

  def nTerms: Int = exps.length

  def isZero: Boolean = nTerms == 0

  def isOne: Boolean = order == 1 && toRational.isOne

  @inline def exponent(i: Int): Int = exps(i)

  @inline def coefficient(i: Int): Rational = coeffs(i)

  def coefficientForExponent(e: Int): Rational = {
    import spire.std.int._
    val ind = spire.math.Searching.search(exps, e)
    if (ind >= 0) coeffs(ind) else Rational.zero
  }

  override def toString: String =
    if (nTerms == 0) "0" else {

      val sb = new StringBuilder

      this match {
        case Cyclo.Quadratic(a, b, d) =>
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
        case _ =>
          val eStr = s"e($order)"
          def baseExp(exp: Int): Unit = {
            sb ++= eStr
            if (exp > 1) { sb ++= "^"; sb ++= exp.toString }
          }

          def leadingTerm(exp: Int, coeff: Rational): Unit =
            if (exp == 0) sb ++= coeff.toString
            else if (coeff.isOne) baseExp(exp)
            else if (coeff == -1) { sb ++= "-"; baseExp(exp) }
            else if (coeff > 0) { sb ++= coeff.toString; sb ++= "*"; baseExp(exp) }
            else if (coeff < 0) { sb ++= "-"; sb ++= coeff.abs.toString; sb ++= "*"; baseExp(exp) }
            else sys.error("Coefficient coeff cannot be zero")

          leadingTerm(exps(0), coeffs(0))

          def followingTerm(exp: Int, coeff: Rational): Unit =
            if (coeff.isOne) { sb ++= " + "; baseExp(exp) }
            else if (coeff == -1) { sb ++= " - "; baseExp(exp) }
            else if (coeff > 0) { sb ++= " + "; sb ++= coeff.toString; sb ++= "*"; baseExp(exp) }
            else if (coeff < 0) { sb ++= " - "; sb ++= coeff.abs.toString; sb ++= "*"; baseExp(exp) }
            else sys.error("Coefficient coeff cannot be zero")

          cforRange(1 until nTerms) { k =>
            followingTerm(exps(k), coeffs(k))
          }
      }
      sb.result()
    }

  override def hashCode = {
    import scala.util.hashing.MurmurHash3._
    val n = exps.length
    var h = arraySeed // why not?
    cforRange(0 until n) { i =>
      h = mix(h, coeffs(i).hashCode * 41 + exps(i))
    }
    finalizeHash(h, n)
  }

  override def equals(any: Any) = any match { // TODO: cooperative equality with Rational, etc...
    case that: Cyclo => this === that
    case _ => false
  }

  /** Returns whether the two cyclotomics `lhs` and `rhs` are equal.
    *
    * This is pretty simple because every cyclotomic has an unique
    * representation, so we just have to compare the terms.
    */
  def ===(rhs: Cyclo): Boolean =
    (lhs.order == rhs.order) && (lhs.nTerms == rhs.nTerms) && {
      cforRange(0 until nTerms) { i =>
        if (lhs.exps(i) != rhs.exps(i)) return false
      }
      cforRange(0 until nTerms) { i =>
        if (lhs.coeffs(i) =!= rhs.coeffs(i)) return false
      }
      true
    }

  def =!=(rhs: Cyclo): Boolean = !(lhs === rhs)

  // TODO: equals, hashCode

  def unary_-(): Cyclo = {
    val newCoeffs = coeffs.clone
    cforRange(0 until newCoeffs.length) { i => newCoeffs(i) = -newCoeffs(i) }
    new Cyclo(order, exps, newCoeffs)
  }

  // arithmetic with Rational r.h.s.

  def +(r: Rational): Cyclo =
    if (r.isZero) this
    else if (nTerms == 0) Cyclo(r)
    else {
      if (exps(0) == 0) {
        val newCoeffs = coeffs.clone
        newCoeffs(0) += r
        new Cyclo(order, exps, newCoeffs)
      } else {
        val n = exps.length
        val newExps = new Array[Int](n + 1)
        val newCoeffs = new Array[Rational](n + 1)
        Array.copy(exps, 0, newExps, 1, n)
        Array.copy(coeffs, 0, newCoeffs, 1, n)
        newExps(0) = 0
        newCoeffs(0) = r
        new Cyclo(order, newExps, newCoeffs)
      }
    }

  def -(r: Rational): Cyclo = lhs + (-r)

  def *(r: Rational): Cyclo =
    if (r.isZero) Cyclo.zero
    else if (r.isOne) lhs
    else if (r == -1) -lhs
    else {
      val newCoeffs = coeffs.clone
      cforRange(0 until newCoeffs.length) { i => newCoeffs(i) *= r }
      new Cyclo(order, exps, newCoeffs)
    }

  def /(r: Rational): Cyclo = lhs * (r.reciprocal)

  // arithmetic with Cyclo r.h.s.

  def +(rhs: Cyclo): Cyclo =
    if (rhs.nTerms > lhs.nTerms) rhs + lhs // take the cyclotomic with less terms as r.h.s.
    else {
      val nl = lhs.order
      val nr = rhs.order
      val n = Cyclo.findCommonField(nl, nr)
      val ml = n / lhs.order
      val mr = n / rhs.order

      val res = WorkCyclo(lhs, n)

      cforRange(0 until rhs.nTerms) { i =>
        res.addTo(rhs.exps(i) * mr, rhs.coeffs(i))
      }

      if (nl % ml != 0 || nr % mr != 0) ConvertToBase(res)

      res.reduceToCyclo(ml * mr)
    }

  def -(rhs: Cyclo): Cyclo = {
    val nl = lhs.order
    val nr = rhs.order
    val n = Cyclo.findCommonField(nl, nr)
    val ml = n / lhs.order
    val mr = n / rhs.order

    val res = WorkCyclo(lhs, n)

    cforRange(0 until rhs.nTerms) { i =>
      res.subTo(rhs.exps(i) * mr, rhs.coeffs(i))
    }

    if (nl % ml != 0 || nr % mr != 0) ConvertToBase(res)

    res.reduceToCyclo(ml * mr)
  }

  def *(rhs: Cyclo): Cyclo =
    if (rhs.nTerms > lhs.nTerms) rhs * lhs // take the cyclotomic with less terms as the right operand
    else if (rhs.nTerms == 1 && rhs.exps(0) == 0) lhs * rhs.coeffs(0)
    else {
      val nl = lhs.order
      val nr = rhs.order
      val n = Cyclo.findCommonField(nl, nr)
      val ml = n / lhs.order
      val mr = n / rhs.order
      val res = WorkCyclo(n)
      cforRange(0 until rhs.nTerms) { k => // loop over the terms of the r.h.s.
        val rc = rhs.coeffs(k)
        val re = (rhs.exps(k) * mr) % n
        if (rc.isOne) {
          cforRange(0 until lhs.nTerms) { i =>
            val newExp = (lhs.exps(i) * ml + re) % n
            res.addTo(newExp, lhs.coeffs(i))
          }
        } else if (rc == -1) {
          cforRange(0 until lhs.nTerms) { i =>
            val newExp = (lhs.exps(i) * ml + re) % n
            res.subTo(newExp, lhs.coeffs(i))
          }
        } else {
          cforRange(0 until lhs.nTerms) { i =>
            val newExp = (lhs.exps(i) * ml + re) % n
            res.addTo(newExp, lhs.coeffs(i) * rc)
          }
        }
      }
      ConvertToBase(res)
      res.reduceToCyclo(ml * mr)
    }

  def /(rhs: Cyclo): Cyclo = this * rhs.reciprocal

  /** Returns the cyclotomic number raised to the power `rhs`. */
  def pow(rhs: Int): Cyclo =
    if (rhs == 0) Cyclo.one // for lhs**0, return 1
    else if (rhs == 1) lhs  // lhs**1 == lhs
    else if (rhs == -1) lhs.reciprocal // lhs**-1 = 1/lhs
    else if (lhs.isRational) Cyclo(lhs.toRational.pow(rhs))
    else if (lhs.nTerms == 1) {
      val res = WorkCyclo(order)
      val newExp = nonNeg(lhs.exps(0)).prodMod(nonNeg(rhs mod order), order) // rhs can be negative
      res.coeffs(newExp) = lhs.coeffs(0).pow(rhs)
      ConvertToBase(res)
      res.reduceToCyclo()
    } else {
      // compute the power by repeated squaring
      @tailrec def loop(base: Cyclo, rem: Int, acc: Cyclo): Cyclo =
        if (rem == 1) base * acc
        else {
          val x = if ((rem & 1) == 1) base * acc else acc
          loop(base * base, rem >>> 1, x)
        }
      if (rhs < 0) { // if exponent is negative, invert the cyclotomic
      val rec = lhs.reciprocal
        loop(rec, -rhs-1, rec)
      } else loop(lhs, rhs-1, lhs)
    }

  /** Returns the multiplicative inverse of the cyclotomic. */
  def reciprocal: Cyclo = { // TODO: reuse same array
    var prd = Cyclo.one
    /* Compute the product of all nontrivial galois conjugates of `lhs`. */
    cforRange(2 until order) { i =>
      if (pos(order).coprimeTo(pos(i))) { // if `i` gives a galois automorphism, apply it
      val res = WorkCyclo(order)
        cforRange(0 until nTerms) { k =>
          res.coeffs((i*exps(k)) % order) = coeffs(k)
        }
        OrderProperties(res)
        if (res.squareFree.get)
          prd *= res.reduceToCyclo(order)
        else {
          ConvertToBase(res)
          prd *= res.reduceToCyclo()
        }
      }
    }
    val norm = (prd * lhs).toRational
    prd / norm
  }

  /** Returns `coefficient(0) + ... + coefficient(nTerms - 1).` */
  def sumOfCoefficients: Rational =
    if (nTerms == 0) Rational.zero
    else if (nTerms == 1) coeffs(0)
    else {
      var sum = coeffs(0)
      cforRange(1 until nTerms) { i =>
        sum += coeffs(i)
      }
      sum
    }

  /** Returns `sum_k (-1)^exponent(k) coefficient(k).` */
  def alternatingSumOfCoefficients: Rational =
    if (nTerms == 0) Rational.zero
    else if (nTerms == 1 && exps(0) % 2 == 0) coeffs(0)
    else if (nTerms == 1) -coeffs(1)
    else {
      var sum = if (exps(0) % 2 == 0) coeffs(0) else -coeffs(0)
      cforRange(1 until nTerms) { i =>
        if (exps(i) % 2 == 0)
          sum += coeffs(i)
        else
          sum -= coeffs(i)
      }
      sum
    }

  /** Image of the cyclotomic under the Galois automorphism given by `ord`. The
    * Galos automorphism is the mapping that takes `e_n` to `e_n^ord`. `ord` may
    * be any integer, of course if it is not relative prime to `order`, the mapping
    * will not be an automophism, though still an endomorphism.
    */
  def galois(ord: Int): Cyclo =
    if (order == 1) lhs // every Galois automorphism fixes the rationals
    else if (ord == 1) lhs // ord == 1 is the identity
    else if (ord == 0) Cyclo(sumOfCoefficients) // compute the sum of entries
    else if (ord < 0) galois(neg(ord) mod order)
    else if (ord >= order) galois(ord % order)
    else if (order % 2 == 0 && ord == order / 2) Cyclo(alternatingSumOfCoefficients)
    else if (pos(ord).coprimeTo(pos(order))) {
      // if `ord` is coprime to `order`, we perform an automorphism that permutes coefficients
      val res = WorkCyclo(order)
      cforRange(0 until nTerms) { i =>
        res.coeffs(nonNeg(exps(i)).prodMod(nonNeg(ord), order)) = coeffs(i)
      }
      OrderProperties(res)
      val cond1 = res.squareFree.get // if the order is square-free
      val cond2 = (ord == order - 1) && order.isOdd // optimize for complex conjugate (odd orders)
      if (cond1 || cond2)
        res.reduceToCyclo(order)
      else { // otherwise, perform base conversion
        ConvertToBase(res)
        res.reduceToCyclo()
      }
    } else {
      // if `ord` is not coprime to `order` (endomorphism), compute it the hard way
      val res = WorkCyclo(order)
      cforRange(0 until nTerms) { i =>
        res.addTo(nonNeg(exps(i)).prodMod(nonNeg(ord), order), coeffs(i))
      }
      OrderProperties(res)
      if (res.squareFree.get)
        res.reduceToCyclo()
      else {
        ConvertToBase(res)
        res.reduceToCyclo()
      }
    }

  def isCyclotomicInteger: Boolean = {
    cforRange(0 until nTerms) { i =>
      if (!coeffs(i).isWhole) return false
    }
    true
  }

  def isRational: Boolean = order == 1

  def toRational: Rational =
    if (order != 1) sys.error(s"The cyclotomic number $this is not rational.")
    else if (nTerms == 0) Rational.zero
    else coeffs(0)

  def conj: Cyclo = galois(-1)

  def isReal: Boolean = this === this.conj

  def real: Cyclo = (this + conj) * oneHalf

  def imag: Cyclo = (conj - this) * Cyclo.i * oneHalf

  def conductor: Int = order // because the order has been reduced

  def hasIntegerCoefficients: Boolean = {
    cforRange(0 until nTerms) { i =>
      if (!coeffs(i).isWhole) return false
    }
    true
  }

  def isGaussianRational = conductor == 1 || conductor == 4

  def isGaussianInteger = isGaussianRational && hasIntegerCoefficients

  def modulusSquared: Cyclo = this * this.conj

  def abs: Opt[Cyclo] = {
    val ms = modulusSquared
    if (ms.isRational) Opt(Cyclo.sqrt(ms.toRational)) else Opt.empty[Cyclo]
  }

  // complex sign function
  def csgn: Opt[Cyclo] =
    if (nTerms == 0) Opt(Cyclo.zero)
    else abs match {
      case Opt(a) => Opt(this / a)
      case _ => Opt.empty[Cyclo]
    }

}

trait CycloEq extends Eq[Cyclo] {

  def eqv(x: Cyclo, y: Cyclo) = x === y

}

trait CycloField extends Field.WithDefaultGCD[Cyclo] {

  def plus(c1: Cyclo, c2: Cyclo) = c1 + c2
  override def minus(c1: Cyclo, c2: Cyclo) = c1 - c2
  def times(c1: Cyclo, c2: Cyclo) = c1 * c2
  def negate(c: Cyclo) = -c
  def zero = Cyclo.zero
  override def fromInt(n: Int) =
    if (n == 0) Cyclo.zero else Cyclo(n)

  def one = Cyclo.one
  override def reciprocal(c: Cyclo) = c.reciprocal
  def div(c1: Cyclo, c2: Cyclo) = c1 / c2


  override def pow(a: Cyclo, n: Int): Cyclo = a.pow(n)
  override def isZero(a: Cyclo)(implicit ev: Eq[Cyclo]) = a.isZero

  override def isOne(a: Cyclo)(implicit ev: Eq[Cyclo]) = a.isOne

}

object Cyclo {

  implicit object algebra extends CycloEq with CycloField

  implicit def viewFromRational(r: Rational): Cyclo = Cyclo(r)

  val maxLimit = Int.MaxValue / 2 - 1

  // TODO: move into an implicit
  private[this] var _softLimit = 1000000

  def softLimit: Int = _softLimit

  def softLimit_=(newSoftLimit: Int): Unit = {
    if (newSoftLimit > maxLimit)
      sys.error(s"You try to set a soft limit $newSoftLimit, greater than the maximal cyclotomic order $maxLimit.")
    _softLimit = newSoftLimit
  }

  /** Find the smallest field size containing cyclotomics of orders `nl` and `nr`. */
  @inline def findCommonField(nl: Int, nr: Int): Int = {
    val common = nonNeg(nl).lcm(nonNeg(nr))
    if (!common.isValidInt || common >= maxLimit)
      sys.error("This computation would require a cyclotomic field too large to be handled")
    else if (common > softLimit)
      sys.error(s"This computation requires a cyclotomic field of degree $common, larger than the current limit of $softLimit. You may restart the comoputation after raising the limit using `Cyclo.setSoftLimit`.")
    else common.toInt
  }

  val zero: Cyclo = new Cyclo(1, Array.empty[Int], Array.empty[Rational])

  val one: Cyclo = new Cyclo(1, Array(0), Array(Rational.one))

  val minusOne: Cyclo = new Cyclo(1, Array(0), Array(-Rational.one))

  val i: Cyclo = e(4)

  def apply(i: Int): Cyclo =
    if (i == 0) Cyclo.zero else new Cyclo(1, Array(0), Array(Rational(i)))

  def apply(r: Rational): Cyclo =
    if (r.isZero) Cyclo.zero else new Cyclo(1, Array(0), Array(r))

  def e(n: Int): Cyclo = // TODO: cache primitive roots ? if so, update Cyclo.pow
    if (n < 1) sys.error("e requires a positive integer")
    else if (n == 1) Cyclo.one
    else if (n == 2) Cyclo(-1)
    else {
      val res = WorkCyclo(n)
      res.addTo(1, Rational.one)
      ConvertToBase(res)
      res.reduceToCyclo()
    }

  def eb(n: Int): Cyclo =
    if (n < 1) sys.error("eb needs a positive integer")
    else if (n % 2 != 1) sys.error("eb needs an odd integer")
    else if (n == 1) zero
    else {
      val en = e(n)
      @tailrec def loop(k: Int, sum: Cyclo): Cyclo =
        if (k > (n - 1) / 2) sum
        else loop(k + 1, sum + en.pow((k * k) % n))
      loop(1, Cyclo.zero)
    }

  val sqrt2: Cyclo = e(8) - e(8).pow(3)

  def sqrt(r: Rational): Cyclo =
    if (r.isWhole) {
      if (!r.numerator.isValidInt) sys.error(s"The square root of ${r.numerator} is too big.")
      sqrt(r.toInt)
    }  else {
      val prd = r.numerator * r.denominator
      if (!prd.isValidInt) sys.error(s"The square root of $r is too complex, and num * den = $prd does not fit in an Int.")
      sqrt(prd.toInt) * Rational(1, r.denominator)
    }

  def sqrt(n: Int): Cyclo =
    if (n.signum == 0) zero
    else if (n.signum < 0) i * sqrtPositiveInt(-n)
    else sqrtPositiveInt(n)

  def sqrtPositiveInt(n: Int): Cyclo =
    if (n < 1) sys.error("sqrtPositiveInt needs a positive integer")
    else {
      val factors = Factors(n) // TODO replace
      val factor = factors.squarePartSqrt
      val squareFreePart = factors.squareFreePart
      (squareFreePart % 4).toInt match {
        case 1 => (eb(squareFreePart) * 2 + 1) * factor
        case 2 => (sqrt2 * sqrtPositiveInt(squareFreePart / 2)) * factor
        case 3 => -i * (eb(squareFreePart) * 2 + 1) * factor
        case _ => sqrtPositiveInt(squareFreePart / 4) * (factor * 2)
      }
    }

  def gaussian(p: Rational, q: Rational): Cyclo = i * q + p

  def polarRev(r: Rational, s: Rational): Cyclo = {
    val p = s.numerator
    val q = s.denominator
    if (!p.isValidInt) sys.error(s"Rational $s is too complex, numerator $p does not fit in an Int.")
    if (!(-p).isValidInt) sys.error(s"Rational $s is too complex, numerator $p does not fit in an Int.")
    if (!q.isValidInt) sys.error(s"Rational $s is too complex, denominator $q does not fit in an Int.")
    if (p.signum >= 0)
      e(q.toInt).pow(p.toInt) * r
    else
      e(q.toInt).pow((-p).toInt).conj * r
  }

  def polarDeg(r: Rational, deg: Rational): Cyclo = polarRev(r, deg / 360)

  def sinDeg(d: Rational): Cyclo = sinRev(d / 360)

  def cosDeg(d: Rational): Cyclo = cosRev(d / 360)

  def sinRev(n: Rational): Cyclo = {
    val nm = n.numerator.abs
    val dn = n.denominator
    if (!nm.isValidInt) sys.error(s"Rational $n is too complex, numerator $nm does not fit in an Int.")
    if (!dn.isValidInt) sys.error(s"Rational $n is too complex, denominator $dn does not fit in an Int.")
    val a = e(dn.toInt).pow(nm.toInt)
    (a.conj - a) * i * oneHalf * dn.signum
  }

  def cosRev(n: Rational): Cyclo = {
    val nm = n.numerator.abs
    val dn = n.denominator
    if (!nm.isValidInt) sys.error(s"Rational $n is too complex, numerator $nm does not fit in an Int.")
    if (!dn.isValidInt) sys.error(s"Rational $n is too complex, denominator $dn does not fit in an Int.")
    val a = e(dn.toInt).pow(nm.toInt)
    (a + a.conj) * oneHalf
  }

  object AsRational {

    def unapply(x: Cyclo): Opt[Rational] = if (x.isRational) Opt(x.toRational) else Opt.empty[Rational]

  }

  object Quadratic {

    /** Returns, if they exist, rationals `a`, `b` and integer `d` such that `x = a + b sqrt(d)`, with d minimal. */
    def unapply(x: Cyclo): Opt[(Rational, Rational, Int)] = x.order match {
      case 1 => Opt((x.toRational, Rational.zero, 1))
      case d =>
        val sd = Cyclo.sqrt(d)
        if (sd.order != x.order) return Opt.empty[(Rational, Rational, Int)]
        val nonZeroExp = if (sd.exponent(0) != 0) sd.exponent(0) else sd.exponent(1)
        val b = x.coefficientForExponent(nonZeroExp) / sd.coefficientForExponent(nonZeroExp)
        val a = x - sd * b
        val factors = Factors(d)
        a match {
          case AsRational(ra) => Opt((ra, b * factors.squarePartSqrt, factors.squareFreePart))
          case _ => Opt.empty[(Rational, Rational, Int)]
        }
    }
  }
}
