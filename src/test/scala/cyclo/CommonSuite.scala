package cyclo

import org.scalacheck.Shrink
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

/**
  * An opinionated stack of traits to improve consistency and reduce
  * boilerplate in Cyclo tests.
  */
trait CommonSuite extends FunSuite with Matchers
  with PropertyChecks
  with Discipline
  with spire.syntax.AllSyntax with spire.std.AnyInstances {

  def noShrink[T] = Shrink[T](_ => Stream.empty)

}
