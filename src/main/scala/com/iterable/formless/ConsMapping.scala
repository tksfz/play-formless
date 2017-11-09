package com.iterable.formless

import play.api.data.{FormError, Mapping}
import play.api.data.validation.Constraint
import shapeless.{::, HList}

class ConsMapping[H, T <: HList](
  hmapping: Mapping[H],
  tmapping: Mapping[T],
  val key: String = "",
  val constraints: Seq[Constraint[H :: T]] = Nil
) extends Mapping[H :: T] {

  private[this] val hfield: Mapping[H] = hmapping.withPrefix(key)
  private[this] val tfields: Mapping[T] = tmapping.withPrefix(key)

  override val mappings: Seq[Mapping[_]] = Seq(this) ++ hfield.mappings ++ tfields.mappings

  override def bind(data: Map[String, String]): Either[Seq[FormError], H :: T] = {
    (hfield.bind(data), tfields.bind(data)) match {
      case (Left(errors1), Left(errors2)) => Left(errors1 ++ errors2)
      case (Left(errors1), _) => Left(errors1)
      case (_, Left(errors2)) => Left(errors2)
      case (Right(s), Right(l)) => Right(s :: l)
    }
  }

  override def unbind(value: H :: T): Map[String, String] = {
    hfield.unbind(value.head) ++ tfields.unbind(value.tail)
  }

  override def unbindAndValidate(value: H :: T): (Map[String, String], Seq[FormError]) = {
    val (m1, e1) = hfield.unbindAndValidate(value.head)
    val (m2, e2) = tfields.unbindAndValidate(value.tail)
    (m1 ++ m2, e1 ++ e2)
  }

  override def withPrefix(prefix: String): ConsMapping[H, T] = {
    addPrefix(prefix) match {
      case Some(newKey) => new ConsMapping(hfield, tfields, newKey, constraints)
      case None => this
    }
  }

  override def verifying(constraints: Constraint[H :: T]*): ConsMapping[H, T] = {
    new ConsMapping(hfield, tfields, key, this.constraints ++ constraints.toSeq)
  }
}
