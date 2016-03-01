package cyclo

import java.util.concurrent.locks.ReentrantLock

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
      if (el ne null) {
        nz += 1
        gcd = nonNeg(gcd).gcd(nonNeg(i))
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
        c.coeffs(i * gcd) = null
      }
      c.order = n / gcd
    }

    c.numberNonZero = nz
    if (allEqual)
      c.allEqualTo = Opt(firstNZ)
    else
      c.allEqualTo = Opt.empty[Rational]
  }

  /** If possible, reduce `c` into the rationals. The properties of the order
    * have already been computed.
    * Returns whether the reduction has been successful.
    */
  def reduceRational(c: WorkCyclo): Boolean = {
    require(c.numberNonZero != -1)
    require(c.phi != -1)
    require(c.squareFree.nonEmpty)
    require(c.numberOfPrimes != -1)
    if (c.numberNonZero == c.phi && c.allEqualTo.nonEmpty && c.squareFree.get) {
      if (c.cleaning)
        cforRange(1 until c.order) { i => c.coeffs(i) = null }
      /* Return as rational `(-1)^{number primes}*{common coefficient}`  */
      if (c.numberOfPrimes % 2 == 0) // by definition, we have `c.allEqualTo ne null`
        c.coeffs(0) = c.allEqualTo.get
      else
        c.coeffs(0) = -c.allEqualTo.get
      c.numberNonZero = 1
      c.order = 1
      OrderProperties(c) // fill with correct values from table
      c.allEqualTo = Opt.empty[Rational]
      true
    } else false
  }

  /* For all primes `p` try to reduce from `Q(e_n)` into `Q(e_{n/p})`.
   * The cyclotomic number must already have been converted into the
   * base.
   * 
   * `hint` must be a divisor of `c.order` and gives a  hint about possible subfields.
   * If a prime `p` divides `hint`, it means that no reduction into a subfield whose order
   * is `n / p` is possible. In the arithmetic functions you can take
   * `lcm(n_l,n_r) / gcd(n_l,n_r) = n / gcd(n_l,n_r)`. If you can not provide
   * such a hint just pass 1.
   * 
   * See GAP cyclotom.c function Cyclotomic.
   * 
   * Returns whether the reduction has been successful.
   */
  def reduceBasis(c: WorkCyclo, hint: Int = 1): Boolean = {
    require(c.numberNonZero > 0) // must be known, and != 0
    require(c.phi != -1)
    val gcd = nonNeg(c.numberNonZero).gcd(nonNeg(c.phi))
    var n = c.order
    var len = c.numberNonZero
    var nn = n // remaining part of order after removing primes one by one
    var p = 3 // current candidate prime
    val els = c.coeffs
    while (p <= nn && (p - 1) <= gcd) {
      if (nn % p == 0) {
        nn /= p
        while (nn % p == 0) nn /= p
        val cond1 = n % (p * p) != 0 // `p` is not a quadratic factor of `n`
        val cond2 = len % (p - 1) == 0 // the number of terms is divisible by `p - 1`
        val cond3 = hint % p != 0 // if `p` divides `hint`, no such reduction is possible
        if (cond1 && cond2 && cond3) {
          /* Tests that coeffs for exponents congruent mod `n/p` are equal */
          var eql = true
          var i = 0
          while (i < n && eql) {
            var cof = els((i + n/p) % n)
            var k = i + 2*n/p
            while (k < i + n && eql) {
              if (cof != els(k % n)) eql = false
              k += n/p
            }
            i += p
          }
          /* If all coeffs for exponents in all classes are equal, reduce */
          if (eql) {
            /* replace every sum of `p-1` terms with expnts congruent
             * to `i*p mod n/p` by the term with exponent `i*p`
             * is just the inverse transformation of 'ConvertToBase' */
            var i = 0
            while (i < n) {
              val cof = if (els((i + n/p) % n) eq null) null else -els((i + n/p) % n)
              els(i) = cof
              var k = i+n/p
              while (k < i + n) {
                els(k%n) = null
                k += n/p
              }
              i += p
            }
            len /= p - 1
            /* Now replace `e_n^{i*p}` by `e_{n/p}^{i}` */
            cforRange(1 until n/p) { i =>
              els(i) = els(i * p)
              els(i * p) = null
            }
            n /= p
          }
        }
      }
      p += 2
    }
    if (c.order != n) {
      c.numberNonZero = len
      c.order = n
      c.phi = -1
      c.squareFree = Opt.empty[Boolean]
      c.numberOfPrimes = -1
      c.allEqualTo = Opt.empty[Rational]
      true
    } else false
  }

}
