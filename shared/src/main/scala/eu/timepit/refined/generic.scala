package eu.timepit.refined

import eu.timepit.refined.InferenceRule.==>
import eu.timepit.refined.generic._
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.product.ToRecord
import shapeless.ops.record.Keys
import shapeless.ops.tuple.ToList
import shapeless.record._

import shapeless.{HList, LabelledGeneric, Witness}

object generic extends GenericPredicates with GenericInferenceRules {

  /** Predicate that checks if a value is equal to `U`. */
  trait Equal[U]

  // P ist z.B. StartsWith[W.`"foo"`.T]
  trait FieldMatches[P]

  /** Predicate that witnesses that the type of a value is a subtype of `U`. */
  trait Subtype[U]

  /** Predicate that witnesses that the type of a value is a supertype of `U`. */
  trait Supertype[U]
}

private[refined] trait GenericPredicates {

  implicit def equalPredicate[T, U <: T](implicit wu: Witness.Aux[U]): Predicate[Equal[U], T] =
    Predicate.instance(_ == wu.value, t => s"($t == ${wu.value})")

  implicit def fieldMatches[T, P, R <: HList, K <: HList](implicit lg: LabelledGeneric.Aux[T, R],
                                              keys: Keys.Aux[R, K], ktl: ToTraversable.Aux[K, List, Any],
                                                        tr: ToRecord.Aux[T, R],
                                                         p: Predicate[P, String]): Predicate[FieldMatches[P], T] = {
    Predicate.instance(t => {
      //val x = tr.apply(t)

      tr.apply(t).keys.toList.map(_.toString.tail).exists(p.isValid)
     // val x = lg.to(t)


      //keys.apply()

      //val keys = Keys[lg.Repr]
      //val x = ktl(keys())
     //t.keys.toList
      //val ks = keys().toList.map(_.toString.tail)
      //ks.exists(p.isValid)

    }, t => "")
  }

  implicit def subtypePredicate[T, U >: T]: Predicate[Subtype[U], T] =
    Predicate.alwaysValid

  implicit def supertypePredicate[T, U <: T]: Predicate[Supertype[U], T] =
    Predicate.alwaysValid
}

private[refined] trait GenericInferenceRules {

  implicit def equalPredicateInference[T, U <: T, P](implicit p: Predicate[P, T], wu: Witness.Aux[U]): Equal[U] ==> P =
    InferenceRule(p.isValid(wu.value), s"equalPredicateInference(${p.show(wu.value)})")
}
