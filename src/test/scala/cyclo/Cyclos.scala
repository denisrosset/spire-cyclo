package cyclo

import org.scalacheck.{Arbitrary, Gen}
import spire.math.Rational

import org.typelevel.discipline.Predicate

object Cyclos {

  implicit object NonZero extends Predicate[Cyclo] {
    override def apply(a: Cyclo): Boolean = !a.isZero
  }

  def genNonZeroCyclo: Gen[Cyclo] = Gen.oneOf(
    for {
      n <- Gen.choose(1, 10)
      d <- Gen.choose(1, 10)
    } yield Cyclo(Rational(n, d)),
    for {
      n <- Gen.choose(-10, -1)
      d <- Gen.choose(1, 10)
    } yield Cyclo(Rational(n, d)),
    Gen.choose(1, 5).map(k => Cyclo.e(k))
  )

  def genSimpleCyclo: Gen[Cyclo] = Gen.oneOf(
    for {
      n <- Gen.choose(-10, 10)
      d <- Gen.choose(1, 10)
    } yield Cyclo(Rational(n, d)),
    Gen.choose(1, 5).map(k => Cyclo.sinRev(Rational(1, k))),
    Gen.choose(1, 5).map(k => Cyclo.e(k))
  )


  implicit def arbCyclo: Arbitrary[Cyclo] = Arbitrary {
    Gen.oneOf(
      genSimpleCyclo,
      genNonZeroCyclo.map(_.reciprocal),
      for(x <- genSimpleCyclo; y <- genNonZeroCyclo) yield x / y,
      for(x <- genSimpleCyclo; y <- genSimpleCyclo) yield x * y,
      for(x <- genSimpleCyclo; y <- genSimpleCyclo) yield x + y,
      for(x <- genSimpleCyclo; y <- genSimpleCyclo) yield x - y
    )
  }

}
