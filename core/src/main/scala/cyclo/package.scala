
import spire.math.Rational

package object cyclo {

  object minusOne {

    def pow(exp: Int): Int = 1 - 2 * (exp % 2)

  }

  val oneHalf = Rational(1, 2)

  def pos(int: Int): PosInt = new PosInt(int)
  
  def neg(int: Int): NegInt = new NegInt(int)

  def nonNeg(int: Int): NonNegInt = new NonNegInt(int)

  implicit def anyInt(int: Int): AnyInt = new AnyInt(int)

  @inline def nullIfZero(r: Rational) = if (r.isZero) null else r

}
