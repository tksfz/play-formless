package com.iterable.formless

import eu.timepit.refined.api.Refined
import play.api.data.Forms
import play.api.data.Forms._
import play.api.data.Mapping
import shapeless.HNil
import shapeless.Poly1

object DefaultsWithNonEmptyText extends Poly1 {
  implicit def caseString = at[String](_ => nonEmptyText)
  implicit def caseByte = at[Byte](_ => byteNumber)
  implicit def caseShort = at[Short](_ => shortNumber)
  implicit def caseInt = at[Int](_ => number)
  implicit def caseLong = at[Long](_ => longNumber)
  implicit def caseBoolean = at[Boolean](_ => boolean)
  implicit def caseOption[T](implicit caseT: Case.Aux[T, Mapping[T]]) = at[Option[T]](_ => optional(caseT.value(null.asInstanceOf[T] :: HNil)))
  implicit def caseSeq[T](implicit caseT: Case.Aux[T, Mapping[T]]) = at[Seq[T]](_ => seq(caseT.value(null.asInstanceOf[T] :: HNil)))
}

object DefaultsWithRefined extends Poly1 {

  implicit def caseRefined[T, P](implicit ev: RefinedConstraint[T, P]) = {
    at[T Refined P](_ => Forms.of[T].verifying(ev()).asInstanceOf[Mapping[T Refined P]])
  }

}
