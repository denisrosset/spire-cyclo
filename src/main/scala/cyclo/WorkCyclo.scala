package cyclo

import spire.math.Rational
import spire.syntax.cfor._
import spire.util.Opt

/** Working mutable cyclotomic number. */
class WorkCyclo(
  var order: Int, // order of the cyclotomic number
  val coeffs: Array[Rational], // array of coefficients, all 0s set to null, array can be larger than needed
  var cleaning: Boolean, // whether the unused parts of coeffs array should be set to null during computations
  // the following is set by OrderProperties, and used by Reduction.reduceBasis,
  var phi: Int = -1, // Euler totient phi(order), or -1 if not computed
  var squareFree: Opt[Boolean] = Opt.empty[Boolean], // is `order` square-free
  var numberOfPrimes: Int = -1, // number of primes in `order`, or -1 if not computed
  var numberNonZero: Int = -1, // number of non-zero elements in coeffs(0 until order), or -1 if not computed
  // used by Reduction.reduceRational
  var allEqualTo: Opt[Rational] = Opt.empty[Rational], // if non-empty, the value that all non-zero coefficients are equal to
  // set by convertToBase
  var inBase: Boolean = false // have the coefficients been reduced in the basis ?
) {

  override def toString = s"WorkCyclo(order = $order, coeffs.toSeq = ${coeffs.toSeq}, cleaning = $cleaning, phi = $phi, squareFree = $squareFree, numberOfPrimes = $numberOfPrimes, numberNonZero = $numberNonZero, allEqualTo = $allEqualTo, inBase = $inBase"

  protected def buildCyclo(): Cyclo = {
    val exps = new Array[Int](numberNonZero)
    val values = new Array[Rational](numberNonZero)
    var ind = 0
    cforRange(0 until order) { i =>
      if (coeffs(i) ne null) {
        exps(ind) = i
        values(ind) = coeffs(i)
        ind += 1
        if (cleaning) coeffs(i) = null
      }
    }
    assert(ind == numberNonZero)
    new Cyclo(order, exps, values)
  }

  // to call after gcdReduction, reduceRational, reduceBasis have been applied
  def reduceToCyclo(hint: Int = 1): Cyclo = {
    Reduction.gcdReduction(this)
    if (numberNonZero == 0) return Cyclo.zero
    OrderProperties(this)
    if (Reduction.reduceRational(this))
      buildCyclo()
    else {
      Reduction.reduceBasis(this, hint)
      buildCyclo()
    }
  }

  def addTo(i: Int, r: Rational): Unit =
    if (!r.isZero) {
      if (coeffs(i) eq null)
        coeffs(i) = r
      else
        coeffs(i) = nullIfZero(coeffs(i) + r)
    }

  def subTo(i: Int, r: Rational): Unit =
    if (!r.isZero) {
      if (coeffs(i) eq null)
        coeffs(i) = -r
      else
        coeffs(i) = nullIfZero(coeffs(i) - r)
    }

}

object WorkCyclo {

  def apply(c: Cyclo): WorkCyclo = {
    val array = new Array[Rational](c.order)
    val zero = Rational.zero
    var last = -1
    cforRange(0 until c.exps.length) { i =>
      val exp = c.exps(i)
      cforRange(last + 1 until exp) { j => array(j) = zero }
      array(exp) = c.coeffs(i)
      last = exp
    }
    cforRange(last + 1 until c.order) { j => array(j) = zero }
    new WorkCyclo(c.order, array, true) // TODO: set to false
  }

  /** Creates a working copy of `c` in the order `n >= c.order`, where `c.order` divides `n`. */
  def apply(c: Cyclo, n: Int): WorkCyclo = {
    assert(c.order <= n)
    val m = n / c.order
    assert(n % c.order == 0)
    val array = new Array[Rational](n)
    cforRange(0 until c.nTerms) { i =>
      array(c.exponent(i) * m) = c.coefficient(i)
    }
    new WorkCyclo(n, array, true) // TODO: false
  }

  def apply(n: Int): WorkCyclo =
    new WorkCyclo(n, new Array[Rational](n), true) // TODO: false

}
