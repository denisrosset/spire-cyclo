package cyclo

import java.lang.Integer.numberOfTrailingZeros

import scala.annotation.tailrec

/** Integer helpers. 
  * 
  * The following assumptions are not checked:
  * 
  * - exponents are always non-negative
  * - modulus arguments are always positive
  */
class AnyInt(val v: Int) extends AnyVal { lhs =>

  /** Modulo operator that guarantees that the result is in [0 .. modulus - 1].
    * Requires `modulus` >= 0.
    */
  @inline def mod(modulus: Int): Int =
    if (v < 0) ((v + 1) % modulus) + modulus - 1 else v % modulus

  @inline def isOdd: Boolean = (v % 2) != 0

  @inline def isEven: Boolean = (v % 2) == 0

}

class PosInt(val v: Int) extends AnyVal { lhs =>

  @inline def coprimeTo(rhs: PosInt): Boolean =
    if (lhs.v == 1 || rhs.v == 1) true
    else {
      var x = lhs.v
      var xz = numberOfTrailingZeros(x)
      var y = rhs.v
      var yz = numberOfTrailingZeros(y)
      if (xz > 0 && yz > 0) false else {
        x = x >> xz
        y = y >> yz
        while (x != y) {
          if (x > y) {
            x -= y
            x >>= numberOfTrailingZeros(x)
          } else {
            y -= x
            y >>= numberOfTrailingZeros(y)
          }
        }
        x == 1
      }
    }

}

class NonNegInt(val v: Int) extends AnyVal { lhs =>

  /** Modulo operator that guarantees that the result is in [0 .. modulus - 1].
    * Requires `modulus` >= 0.
    */
  @inline def mod(rhs: Int): Int = lhs.v % rhs

  /** GCD of two non-negative integers.
    * 
    * Inspired by Spire's math.gcd.
    */
  @inline def gcd(rhs: NonNegInt): Int = {
    if (lhs.v == 0) return rhs.v
    if (lhs.v == 1) return 1
    if (rhs.v == 0) return lhs.v
    if (rhs.v == 1) return 1

    var x = lhs.v
    var xz = numberOfTrailingZeros(x)
    x = x >> xz

    var y = rhs.v
    var yz = numberOfTrailingZeros(y)
    y = y >> yz

    while (x != y) {
      if (x > y) {
        x -= y
        x >>= numberOfTrailingZeros(x)
      } else {
        y -= x
        y >>= numberOfTrailingZeros(y)
      }
    }

    if (xz < yz) x << xz else x << yz
  }

  @inline def lcm(rhs: NonNegInt): Long =
    (lhs.v / gcd(rhs)).toLong * rhs.v.toLong

  @inline def prodMod(rhs: NonNegInt, modulus: Int): Int =
    ((lhs.v.toLong * rhs.v.toLong) % modulus.toLong).toInt

  def modPow(exponent: Int, modulus: Int): Int =
    if (modulus == 1) 0
    else if (exponent == 0) 1
    else if (exponent == 1) v % modulus
    else {
      @tailrec def loop(b: Long, k: Int, acc: Long): Long =
        if (k == 1)
          (b * acc) % modulus
        else {
          val x = if ((k & 1) == 1) (b * acc) % modulus else acc
          loop((b * b) % modulus, k >>> 1, x)
        }
      val bm = lhs.v % modulus
      loop(bm, exponent - 1, bm).toInt
    }

  def pow(exponent: NonNegInt): Int =
    if (exponent.v == 0) 1
    else if (exponent.v == 1) lhs.v
    else {
      @tailrec def loop(b: Long, k: Long, acc: Long): Long =
        if (k == 1)
          b * acc
        else {
          val x = if ((k & 1) == 1) {
            val ba = b * acc
            assert(ba.isValidInt)
            ba
          } else acc
          val bb = b * b
          assert(bb.isValidInt)
          loop(bb, k >>> 1, x)
        }
      val res = loop(lhs.v, exponent.v - 1, lhs.v)
      assert(res.isValidInt)
      res.toInt
    }

}

class NegInt(val v: Int) extends AnyVal {

  /** Modulo operator that guarantees that the result is in [0 .. modulus - 1].
    * Requires `modulus` >= 0.
    */
  @inline def mod(modulus: Int): Int = ((v + 1) % modulus) + modulus - 1

}
