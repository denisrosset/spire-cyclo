package cyclo

import org.scalacheck.{Arbitrary, Gen}
import spire.math.Rational

import org.typelevel.discipline.Predicate

object Cyclos {

  implicit object NonZero extends Predicate[Cyclo] {
    override def apply(a: Cyclo): Boolean = !a.isZero
  }

  implicit def arbCyclo: Arbitrary[Cyclo] = Arbitrary {
    Gen.oneOf(
      for {
        n <- Gen.choose(-10, 10)
        d <- Gen.choose(1, 10)
      } yield Cyclo(Rational(n, d)),
      Gen.choose(1, 20).map(k => Cyclo.sinRev(k)),
      Gen.choose(1, 20).map(k => Cyclo.e(k))
    )
  }

}
