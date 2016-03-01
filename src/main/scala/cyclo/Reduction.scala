package cyclo

import java.util.concurrent.atomic.AtomicBoolean

import spire.math.Rational
import spire.syntax.cfor._
import spire.util.Opt

object Reduction {

  /** Step1: if all exps are divisible by `k > 1`, then replace all `e_n^i` by `e_{n/k}^{i/k}`.
    * 
    * This is the only way a prime whose square divides $n$ could reduce.
    * 
    * Also checks whether all elements are equal, and computes the number of non-zero
    * elements.
    */
  def gcdReduction(c: WorkCyclo): Unit = {
    var nz = 0
    var gcd = c.order
    var firstNZ: Rational = null // first non-zero element in `c`
    var allEqual = true
    val n = c.order
    cforRange(0 until n) { i =>
      val el = c.coeffs(i)
      if (!el.isZero) {
        nz += 1
        gcd = gcdNonNeg(gcd, i)
        if (allEqual) {
          if (firstNZ eq null)
            firstNZ = el
          else if (el != firstNZ)
            allEqual = false
        }
      }
    }

    if (gcd > 1) {
      cforRange(1 until n / gcd) { i =>
        c.coeffs(i) = c.coeffs(i * gcd)
        c.coeffs(i * gcd) = Rational.zero
      }
      c.order = n / gcd
    }

    c.numberNonZero = nz
    c.allEqual = Opt(allEqual)
  }

  val computing = new AtomicBoolean(false)
  var lastOrder: Int = 1
  var lastPhi = 1
  var lastSquareFree = true
  var lastNumberOfPrimes = 0

  def computeOrderProperties(c: WorkCyclo): Unit = {

  }

}
