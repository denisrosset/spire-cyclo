import scala.collection.immutable.SortedMap
import scala.collection.generic.CanBuildFrom

import spire.math.{Rational, SafeLong}

package object cyclo {

  type Integer = SafeLong
  final val Integer = SafeLong

  type Coeffs = SortedMap[Integer, Rational]

  object Coeffs {

    def apply(elements: (Integer, Rational)*): Coeffs =
      SortedMap[Integer, Rational](elements.filterNot(_._2.isZero): _*)

    def empty: Coeffs = SortedMap.empty[Integer, Rational]

  }

  implicit class RichSortedMap[K, V](m: SortedMap[K, V]) {

    def mapKeys[K1](f: K => K1)(implicit bf: CanBuildFrom[SortedMap[K, V], (K1, V), SortedMap[K1, V]]): SortedMap[K1, V] =
      m.map { case (k, v) => (f(k), v) }

    def insertWith[V1 >: V](k: K, v: V1, f: (V1, V1) => V1): SortedMap[K, V1] =
      if (m.isDefinedAt(k))
        m.updated(k, f(m(k), v))
      else
        m.updated(k, v)

    def unionWith[V1 >: V](m1: SortedMap[K, V], f: (V1, V1) => V1)(implicit ev: Ordering[K]): SortedMap[K, V1] = {
      val allKeys = m.keySet ++ m1.keySet
      val mb = SortedMap.newBuilder[K, V1]
      for (k <- allKeys) {
        (m.get(k), m1.get(k)) match {
          case (Some(v), Some(v1)) => mb += ((k, f(v, v1)))
          case (None, Some(v1)) => mb += ((k, v1))
          case (Some(v), None) => mb += ((k, v))
          case (None, None) => sys.error("Never happens")
        }
      }
      mb.result()
    }

  }

}
