package com.iterable.formless

import play.api.data.Mapping
import shapeless.{HNil, Poly1}

object DefaultsWithNonEmptyText extends Poly1 {
  import play.api.data.Forms._
  implicit def caseString = at[String](_ => nonEmptyText)
  implicit def caseByte = at[Byte](_ => byteNumber)
  implicit def caseShort = at[Short](_ => shortNumber)
  implicit def caseInt = at[Int](_ => number)
  implicit def caseLong = at[Long](_ => longNumber)
  implicit def caseBoolean = at[Boolean](_ => boolean)
  implicit def caseOption[T](implicit caseT: Case.Aux[T, Mapping[T]]) = at[Option[T]](_ => optional(caseT.value(null.asInstanceOf[T] :: HNil)))
  implicit def caseSeq[T](implicit caseT: Case.Aux[T, Mapping[T]]) = at[Seq[T]](_ => seq(caseT.value(null.asInstanceOf[T] :: HNil)))
}
