package cyclo

import spire.algebra.{Eq, Field, Order}
import spire.math.Rational

final class RealCyclo protected[cyclo](val underlying: Cyclo)

trait RealCycloOrder extends Order[RealCyclo] {

  def compare(x: RealCyclo, y: RealCyclo): Int = ???

}

trait RealCycloField extends Field[RealCyclo] {
  def plus(c1: RealCyclo, c2: RealCyclo) = new RealCyclo(c1.underlying + c2.underlying)
  override def minus(c1: RealCyclo, c2: RealCyclo) = new RealCyclo(c1.underlying - c2.underlying)
  def times(c1: RealCyclo, c2: RealCyclo) = new RealCyclo(c1.underlying * c2.underlying)
  def negate(c: RealCyclo) = new RealCyclo(-c.underlying)
  def zero = RealCyclo.zero
  override def fromInt(n: Int) =
    if (n == 0) RealCyclo.zero else RealCyclo(n)

  def one = RealCyclo.one
  override def reciprocal(c: RealCyclo) = new RealCyclo(c.underlying.reciprocal)
  def div(c1: RealCyclo, c2: RealCyclo) = new RealCyclo(c1.underlying / c2.underlying)


  override def pow(a: RealCyclo, n: Int): RealCyclo = new RealCyclo(a.underlying.pow(n))
  override def lcm(a: RealCyclo, b: RealCyclo): RealCyclo = new RealCyclo(a.underlying * b.underlying)
  override def isZero(a: RealCyclo)(implicit ev: Eq[RealCyclo]) = a.underlying.isZero

  def gcd(c1: RealCyclo, c2: RealCyclo): RealCyclo = RealCyclo.one
  def mod(c1: RealCyclo, c2: RealCyclo): RealCyclo = RealCyclo.zero
  def quot(c1: RealCyclo, c2: RealCyclo): RealCyclo = div(c1, c2)

  override def isOne(a: RealCyclo)(implicit ev: Eq[RealCyclo]) = a.underlying.isOne
}

object RealCyclo {

  implicit object algebra extends RealCycloOrder with RealCycloField

  def zero = new RealCyclo(Cyclo.zero)

  def one = new RealCyclo(Cyclo.one)

  def apply(i: Int): RealCyclo = new RealCyclo(Cyclo(i))

  def apply(r: Rational): RealCyclo = new RealCyclo(Cyclo(r))

  def real(c: Cyclo): RealCyclo = new RealCyclo(c.real)

  def imag(c: Cyclo): RealCyclo = new RealCyclo(c.imag)

  val sqrt2: RealCyclo = new RealCyclo(Cyclo.sqrt2)

  def sqrt(r: Rational): RealCyclo =
    if (r < 0) sys.error("Only square root of nonnegative numbers are real")
    else new RealCyclo(Cyclo.sqrt(r))

  def sqrt(n: Int): RealCyclo =
    if (n < 0) sys.error("Only square root of nonnegative numbers are real")
    else new RealCyclo(Cyclo.sqrt(n))

  def sinDeg(d: Rational): RealCyclo = new RealCyclo(Cyclo.sinDeg(d))

  def cosDeg(d: Rational): RealCyclo = new RealCyclo(Cyclo.cosDeg(d))

  def sinRev(n: Rational): RealCyclo = new RealCyclo(Cyclo.sinRev(n))

  def cosRev(n: Rational): RealCyclo = new RealCyclo(Cyclo.cosRev(n))

}
