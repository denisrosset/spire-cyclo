package cyclo

import scala.annotation.tailrec

import spire.algebra.Ring
import spire.algebra.Sign
import Sign.{Zero, Positive, Negative}
import spire.std.int._
import spire.syntax.cfor._
import spire.syntax.nroot._

case class Factors(factors: Map[Int, Int], sign: Sign) {

  private def prod(m: Map[Int, Int]): Int =
    m.foldLeft(1) { case (t, (p, e)) => t * Ring[Int].prodn(p, e) }

  def isSquareFree = factors.values.forall(_ <= 1)

  def squarePartSqrt: Int = // TODO: optimize
    factors.foldLeft(1) {
      case (acc, (fact, exp)) => acc * Ring[Int].prodn(fact, exp / 2)
    }

  def squareFreePart: Int = // TODO: optimize
    factors.foldLeft(1) {
      case (acc, (fact, exp)) if exp % 2 == 1 => acc * fact
      case (acc, _) => acc
    }

  def squareFreeOddFactors: Iterable[Int] =
    factors.keys.filter(fact => fact > 2 && factors(fact) == 1)

  def numberOfPrimeFactors = factors.size

  lazy val value: Int = sign match {
    case Positive => prod(factors)
    case Zero => 0
    case Negative => -prod(factors)
  }

  lazy val phi: Int = {
    @tailrec def loop(acc: Int, it: Iterator[Int]): Int =
      if (it.hasNext) {
        val p = it.next
        loop(acc / p * (p - 1), it)
      } else acc
    loop(value, factors.keysIterator)
  }

  override def toString(): String = {
    def terms =
      if (factors.isEmpty) "1"
      else factors.toSeq.sorted.map {
        case (p, 1) => s"$p"
        case (p, e) => s"$p^$e"
      }.mkString(" * ")
    sign match {
      case Positive => s"($terms)"
      case Zero => "(0)"
      case Negative => s"-($terms)"
    }
  }

  def uniqueFactors: Set[Int] = factors.keySet

}

// taken and translated from https://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Java
object Factors {

  def apply(n: Int): Factors = factorWheelDivision(n)

  val zero = Factors(Map.empty, Zero)
  val one = Factors(Map.empty, Positive)


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

  /**
    * Factor the given integer using trial division with a wheel.
    *
    * This is slightly faster than basic trial divison (about 30% or
    * so). It's still mostly appropriate for small-ish numbers.
    * 
    * Taken from Spire and specialized for Int.
    */
  def factorWheelDivision(n0: Int): Factors = {
    if (n0 == 0) return Factors.zero

    val n = n0.abs
    val sign = Sign(n0.signum)
    if (n == 1) return Factors(Map.empty[Int, Int], sign)

    val facts = scala.collection.mutable.Map.empty[Int, Int]
    var x = n
    val (x1, e1) = findPowers(x, 2)
    if (e1 > 0) {
      facts(2) = e1
      x = x1
    }

    cfor(3)(_ < 30 && x > 1, _ + 2) { b =>
      val (x2, e2) = findPowers(x, b)
      if (e2 > 0) {
        facts(b) = e2
        x = x2
      }
    }

    var limit = x.sqrt
    var b = 31
    var i = 0
    val offsets = Array(2, 2, 2, 4, 2, 4, 2, 4, 6, 2)
    while (b <= limit && x > 1) {
      val (x2, e2) = findPowers(x, b)
      if (e2 > 0) {
        facts(b) = e2
        x = x2
        limit = x.sqrt
      }
      b += offsets(i)
      i = (i + 1) % 10
    }

    if (x > 1) facts(x) = 1
    Factors(facts.toMap, sign)
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

  def intPow(base: Int, exponent: Int): Int =
    if (exponent == 0) 1
    else if (exponent == 1) base
    else {
      @tailrec def loop(b: Int, k: Int, acc: Int): Int =
        if (k == 1)
          b * acc
        else {
          val x = if ((k & 1) == 1) b * acc else acc
          loop(b * b, k >>> 1, x)
        }
      loop(base, exponent - 1, base)
    }

  private def findPowers(x0: Int, b: Int): (Int, Int) = {
    var x = x0
    var e = 0
    while (x > 1 && x % b == 0) { e += 1; x = x / b }
    (x, e)
  }


}
