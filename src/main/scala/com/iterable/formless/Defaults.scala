package com.iterable.formless

import eu.timepit.refined.api.Refined
import play.api.data.Forms
import play.api.data.Forms._
import play.api.data.Mapping
import play.api.data.format.Formatter
import shapeless.HNil
import shapeless.Poly1

trait BaseDefaults extends Poly1 {
  implicit def caseByte = at[Byte](_ => byteNumber)
  implicit def caseShort = at[Short](_ => shortNumber)
  implicit def caseInt = at[Int](_ => number)
  implicit def caseLong = at[Long](_ => longNumber)
  implicit def caseBoolean = at[Boolean](_ => boolean)
  implicit def caseOption[T](implicit caseT: Case.Aux[T, Mapping[T]]) = at[Option[T]](_ => optional(caseT.value(null.asInstanceOf[T] :: HNil)))
  implicit def caseSeq[T](implicit caseT: Case.Aux[T, Mapping[T]]) = at[Seq[T]](_ => seq(caseT.value(null.asInstanceOf[T] :: HNil)))
}

trait DefaultsWithNonEmptyText extends BaseDefaults {
  implicit def caseString = at[String](_ => nonEmptyText)
}

object DefaultsWithNonEmptyText extends DefaultsWithNonEmptyText

trait DefaultsWithEmptyText extends BaseDefaults {
  implicit def caseString = at[String](_ => text)
}

object DefaultsWithEmptyText extends DefaultsWithEmptyText

object DefaultsWithRefined extends DefaultsWithEmptyText {

  implicit def caseRefined[T, P](implicit
    format: Formatter[T],
    rc: RefinedConstraint[T, P]
  ): Case.Aux[T Refined P, Mapping[T Refined P]] = {
    at[T] { _ =>
      Forms.of[T]
        .verifying(rc.constraint)
        .transform[T Refined P](Refined.unsafeApply, _.value)
    }.asInstanceOf[Case.Aux[T Refined P, Mapping[T Refined P]]]
  }

}
