package cyclo

import scala.annotation.tailrec

// taken and translated from https://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Java
object Factors {

  val primesToTest = Array(2, 3, 5, 7, 11, 13, 17)

//  val strongPseudoprime = SafeLong("3215031751")

  implicit class RichInt(val int: Int) extends AnyVal {

    def lowestSetBit: Int =
      if (int == 0) -1
      else java.lang.Integer.numberOfTrailingZeros(int)

  }

  def isPrime(n: Int): Boolean = n match {
    case 1 | 4 | 6 | 8 => false
    case 2 | 3 | 5 | 7 => true
    case _ =>
      val nTests =
        if (n <= 1373653) 3
        else if (n <= 25326001) 4
        else 5 // <= 118670087467
               // 6 <= 2152302898747
               // 7 <= 3474749660383 (for larger integers)

      //      if (n == strongPseudoprime) return false
      val d = n - 1
      val s = d.lowestSetBit
      @tailrec def testPrimes(k: Int): Boolean =
        if (k == nTests) true else {
          val a = primesToTest(k)
          if (tryComposite(a, d >>> s, n, s)) false else testPrimes(k + 1)
        }
      testPrimes(0)
  }

  def tryComposite(a: Int, d: Int, n: Int, s: Int): Boolean =
    if (modPow(a, d, n) == 1) false else {
      @tailrec def loop(i: Int): Boolean =
        if (i == s) true
        else if (modPow(a, (1 << i) * d, n) == n - 1) false
        else loop(i + 1)
      loop(0)
    }

  def modPow(base: Int, exponent: Int, modulus: Int): Int =
    if (modulus == 1) 0
    else if (exponent == 0) 1
    else if (exponent == 1) base % modulus
    else {
      @tailrec def loop(b: Long, k: Int, acc: Long): Long =
        if (k == 1)
          (b * acc) % modulus
        else {
          val x = if ((k & 1) == 1) (b * acc) % modulus else acc
          loop((b * b) % modulus, k >>> 1, x)
        }
      val bm = base % modulus
      loop(bm, exponent - 1, bm).toInt
    }

}
