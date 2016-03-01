package cyclo

import spire.math.Rational
import spire.syntax.cfor._
import spire.util.Opt

/** Working mutable cyclotomic number. */
class WorkCyclo(
  var order: Int, // order of the cyclotomic number
  val coeffs: Array[Rational], // array of coefficients; can be larger (the array is resized only at the end)
  var phi: Int = -1, // Euler totient phi(order), or -1 if not computed
  var squareFree: Opt[Boolean] = Opt.empty[Boolean], // is `order` square-free
  var numberOfPrimes: Int = -1, // number of primes in `order`, or -1 if not computed
  var numberNonZero: Int = -1, // number of non-zero elements in coeffs(0 until order), or -1 if not computed
  var allEqual: Opt[Boolean] = Opt.empty[Boolean], // are all elements are equal
  var inBase: Boolean = false // have the coefficients been reduced in the basis ?
)

object WorkCyclo {

  def apply(c: Cyclo): WorkCyclo = {
    val array = new Array[Rational](c.order)
    val zero = Rational.zero
    var last = -1
    cforRange(0 until c.exps.length) { i =>
      val exp = c.exps(i)
      cforRange(last + 1 until exp) { j => array(j) = zero }
      array(exp) = c.values(i)
      last = exp
    }
    cforRange(last + 1 until c.order) { j => array(j) = zero }
    new WorkCyclo(c.order, array)
  }

}
