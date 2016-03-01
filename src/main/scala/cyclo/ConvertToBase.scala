package cyclo

import java.lang.Integer.lowestOneBit

import spire.math.Rational
import spire.util.Opt


/**
  * Converts the given cyclotomic from the cyclotomic field of `n`-th roots
  * of unity into the base form. This means that it replaces every root `e_n^i`
  * that does not belong to the  base by a sum of other roots that do.
  *
  * Suppose that $c*e_n^i$ appears in the cyclotomic but that `e_n^i` does not lie
  * in the base. This happens because, for some prime `p` dividing `n`, with
  * maximal power `q`, `i \in (n/q)*[-(q/p-1)/2..(q/p-1)/2]` mod `q`.
  *
  * We take the identity  `1+e_p+e_p^2+..+e_p^{p-1}=01, write it using `n`-th
  * roots of unity, `0=1+e_n^{n/p}+e_n^{2n/p}+..+e_n^{(p-1)n/p}` and multiply
  * it by `e_n^i`, `0=e_n^i+e_n^{n/p+i}+e_n^{2n/p+i}+..+e_n^{(p-1)n/p+i}`.
  * Now we subtract `c` times the left hand side from the given cyclotomic.
  *
  * If `p^2` does not divide `n` then the roots that are not in the base
  * because of `p` are those  whose exponent is divisible by `p`. But `n/p`
  * is not divisable by `p`, so neither of the exponent `k*n/p+i, k=1..p-1`
  * is divisible by `p`, so those new roots are acceptable w.r.t. `p`.
  *
  * A similar argument shows that the new roots are also acceptable w.r.t.
  * `p` even if `p^2` divides $n$...
  *
  * Note that the new roots might still not lie in the case because of some
  * other prime `p2`. However, because `i = k*n/p+i mod p2`, this can only
  * happen if `e_n^i` did also not lie in the base because of `p2`. So if we
  * remove all roots that lie in the base because of $p$, the later steps,
  * which remove the roots that are not in the base because of larger primes,
  * will not add new roots that do not lie in the base because of `p` again.
  *
  * For an example, suppose 'c' is `e_{45}+e_{45}^5 = e+e^5`. 
  * `e^5` does not lie in the  base  because `5 \in 5*[-1,0,1] mod 9` and also
  * because it is divisible by 5. After subtracting `e^5*(1+e_3+e_3^2) =
  * e^5+e^{20}+e^{35}` from  'c' we get `e-e^{20}-e^{35}`. Those two roots
  * are still not in the base because of 5. But after subtracting
  * `-e^{20}*(1+e_5+e_5^2+e_5^3+e_5^4)=-e^{20}-e^{29}-e^{38}-e^2-e^{11}` and
  * `-e^{35}*(1+e_5+e_5^2+e_5^3+e_5^4)=-e^{35}-e^{44}-e^8-e^{17}-e^{26}`  we
  * get `e+e^{20}+e^{29}+e^{38}+e^2+e^{11}+e^{35}+e^{44}+e^8+e^{17}+e^{26}`,
  * which contains only roots that lie in the base.
  *
  * 'ConvertToBase' and the methods in 'Reduction' are the functions that know
  * about the structure of the base. The `Eq` instance only need the property
  * that the representation of all cyclotomic integers is unique. All other
  * functions don't even require that cyclotomics are written as a linear
  * combination of linear independent roots, they would work also if
  * cyclotomic integers were written as polynomials in `e_n`.
  *
  * The inner loops in this function have been duplicated to avoid using the
  * modulo ('%') operator to reduce the exponents into  the  range  `0..n-1`.
  */
object ConvertToBase {

  def apply(c: WorkCyclo): Unit = {
    val els = c.coeffs
    val n = c.order
    var nn = n // remaining part after removing primes

    // first handle the prime factor 2
    if (n % 2 == 0) {
      val q = lowestOneBit(n)
      nn /= q

      /* Get rid of all terms `e^{a*q+b*(n/q)}, a=0..(n/q)-1, b=q/2..q-1` */
      var i = 0
      while (i < n) {
        var t = i + (n/q)*(q-1) + n/q  /* end   (n <= t < 2n)     */
        var k = i + (n/q)*(q/2)        /* start (0 <= k <= t)     */
        while (k < n) {
          if (els(k) ne null) {
            val l = (k + n/2) % n
            // perform els(l) -= els(k) and els(k) = 0
            if (els(l) eq null)
              els(l) = -els(k)
            else
              els(l) = nullIfZero(els(l) - els(k))
            els(k) = null
          }
          k += n/q
        }
        t -= n                         /* end   (0 <= t <  n)     */
        k -= n                         /* cont. (0 <= k     )     */
        while (k < t) {
          if (els(k) ne null) {
            val l = (k + n/2) % n
            if (els(l) eq null)
              els(l) = -els(k)
            else
              els(l) = nullIfZero(els(l) - els(k))
            els(k) = null
          }
          k += n/q
        }
        i += q
      }
    }

    // now handle the odd primes
    var p = 3
    while (p <= nn) {
      if (nn % p == 0) {
        var q = p
        nn /= p
        while (nn % p == 0) {
          nn /= p
          q *= p
        }

        /* Get rid of `e^{a*q+b*(n/q)}, a=0..(n/q)-1, b=-(q/p-1)/2..(q/p-1)/2` */
        var i = 0
        while (i < n) {
          var t = i + (n/p-n/q)/2    /* end   (n   <= t < 2n) */
          var k = i - (n/p-n/q)/2    /* start (t-n <= k <= t) */
          if (n > i + (n/p-n/q)/2) {
            t += n
            k += n
          }
          while (k < n) {
            if (els(k) ne null) {
              var l = k + n/p
              while (l < k+n) {
                if (els(l%n) eq null)
                  els(l%n) = -els(k)
                else
                  els(l%n) = nullIfZero(els(l%n) - els(k))
                l += n/p
              }
              els(k) = null
            }
            k += n/q
          }
          t -= n                     /* end   (0   <= t <  n)           */
          k -= n                     /* start (0   <= k     )           */
          while (k <= t) {
            if (els(k) ne null) {
              var l = k + n/p
              while (l < k+n) {
                if (els(l%n) eq null)
                  els(l%n) = -els(k)
                else
                  els(l%n) = nullIfZero(els(l%n) - els(k))
                l += n/p
              }
              els(k) = null
            }
            k += n/q
          }
          i += q
        }
      }
      p += 2
    }
    c.numberNonZero = -1
  }

}
