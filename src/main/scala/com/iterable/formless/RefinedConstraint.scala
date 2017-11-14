package com.iterable.formless

import eu.timepit.refined.boolean.And
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.boolean.Or
import eu.timepit.refined.collection.Empty
import eu.timepit.refined.collection.MaxSize
import eu.timepit.refined.collection.MinSize
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.numeric.Less
import eu.timepit.refined.numeric.Modulo
import eu.timepit.refined.string.MatchesRegex
import play.api.data.validation.Constraint
import play.api.data.validation.Constraints
import play.api.data.validation.Invalid
import play.api.data.validation.Valid
import play.api.data.validation.ValidationError
import shapeless.Nat
import shapeless.Witness

trait RefinedConstraint[T, P] {
  def apply: Constraint[T]
}

object RefinedConstraint {

  // boolean //

  implicit def not[T, P](implicit rc: RefinedConstraint[T, P]): RefinedConstraint[T, Not[P]] = {
    val name = "refined.not"
    new RefinedConstraint[T, Not[P]] {
      def apply: Constraint[T] = {
        Constraint(name) { t =>
          val c = rc()
          c(t) match {
            case Valid => Invalid(ValidationError(name, c.name, c.args: _*))
            case Invalid(_) => Valid
          }
        }
      }
    }
  }

  implicit def and[T, P1, P2](implicit
    rc1: RefinedConstraint[T, P1],
    rc2: RefinedConstraint[T, P2]
  ): RefinedConstraint[T, P1 And P2] = {
    new RefinedConstraint[T, P1 And P2] {
      def apply: Constraint[T] = {
        Constraint("refined.and") { t =>
          (rc1()(t), rc2()(t)) match {
            case (Valid, Valid) => Valid
            case (Valid, inv @ Invalid(_)) => inv
            case (inv @ Invalid(_), Valid) => inv
            case (inv1 @ Invalid(_), inv2 @ Invalid(_)) => inv1 ++ inv2
          }
        }
      }
    }
  }

  implicit def or[T, P1, P2](implicit
    rc1: RefinedConstraint[T, P1],
    rc2: RefinedConstraint[T, P2]
  ): RefinedConstraint[T, P1 Or P2] = {
    new RefinedConstraint[T, P1 Or P2] {
      def apply: Constraint[T] = {
        Constraint("refined.or") { t =>
          rc1()(t) match {
            case Valid => Valid
            case inv1 @ Invalid(_) => rc2()(t) match {
              case Valid => Valid
              case inv2 @ Invalid(_) => inv1 ++ inv2
            }
          }
        }
      }
    }
  }

  // numeric //

  implicit def less[N]: RefinedConstraint[Int, Less[N]] = {
    new RefinedConstraint[Int, Less[N]] {
      def apply: Constraint[Int] = Constraints.max(Nat.toInt[N], strict = true)
    }
  }

  implicit def greater[N]: RefinedConstraint[Int, Greater[N]] = {
    new RefinedConstraint[Int, Greater[N]] {
      def apply: Constraint[Int] = Constraints.min(Nat.toInt[N], strict = true)
    }
  }

  implicit def modulo[N, O]: RefinedConstraint[Int, Modulo[N, O]] = {
    val name = "refined.modulo"
    new RefinedConstraint[Int, Modulo[N, O]] {
      def apply: Constraint[Int] = {
        Constraint("refined.modulo") { t =>
          if (t % Nat.toInt[N] == Nat.toInt[O]) Valid else Invalid(ValidationError(name))
        }
      }
    }
  }

  // string //

  implicit def empty: RefinedConstraint[String, Empty] = {
    val name = "refined.empty"
    new RefinedConstraint[String, Empty] {
      def apply: Constraint[String] = {
        Constraint(name) { t =>
          if (t.isEmpty) Valid else Invalid(ValidationError(name))
        }
      }
    }
  }

  implicit def nonEmpty: RefinedConstraint[String, NonEmpty] = {
    new RefinedConstraint[String, NonEmpty] {
      def apply: Constraint[String] = Constraints.nonEmpty
    }
  }

  implicit def minSize[N]: RefinedConstraint[String, MinSize[N]] = {
    new RefinedConstraint[String, MinSize[N]] {
      def apply: Constraint[String] = Constraints.minLength(Nat.toInt[N])
    }
  }

  implicit def maxSize[N]: RefinedConstraint[String, MaxSize[N]] = {
    new RefinedConstraint[String, MaxSize[N]] {
      def apply: Constraint[String] = Constraints.maxLength(Nat.toInt[N])
    }
  }

  implicit def matchesRegex[S <: String](implicit
    w: Witness.Aux[S]
  ): RefinedConstraint[String, MatchesRegex[S]] = {
    new RefinedConstraint[String, MatchesRegex[S]] {
      def apply: Constraint[String] = Constraints.pattern(w.value.r)
    }
  }

}
