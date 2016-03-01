
import java.lang.Long.numberOfTrailingZeros

package object cyclo {

  /** GCD of two non-negative integers.
    * 
    * Inspired by Spire's math.gcd.
    */
  @inline def gcdNonNeg(_x: Int, _y: Int): Int = {
    if (_x == 0) return _y
    if (_x == 1) return 1
    if (_y == 0) return _x
    if (_y == 1) return 1

    var x = _x
    var xz = numberOfTrailingZeros(x)
    x = x >> xz

    var y = _y
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

}
