package org.tksfz.formless

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraint
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.Align

trait Mapper[T] {
  type Out
  def apply(t: T): Mapping[Out]
}

object Mapper {

  type Aux[A, B] = Mapper[A] { type Out = B }

  def get[L <: HList](l: L)(implicit mapper: Mapper[L]) = mapper.apply(l)

  /**
    * Instances for Mappers that _accept_ records with key ->> mapper
    * and _output_ mappings of HList records
    */

  /**
    * Mapper for record elements that supply the Mapping[T] directly:
    *     'foo ->> nonEmptyText
    */
  implicit def h1[K <: Symbol, T]
  (implicit wk: Witness.Aux[K]): Mapper.Aux[FieldType[K, Mapping[T]], FieldType[K, T]] = new Mapper[FieldType[K, Mapping[T]]] {
    type Out = FieldType[K, T]
    override def apply(t: FieldType[K, Mapping[T]]): Mapping[FieldType[K, T]] = {
      // Since FieldType[K, V] == V tagged with K, the transformation is trivial
      t.withPrefix(wk.value.name).transform(t => t.asInstanceOf[FieldType[K, T]], identity)
    }
  }

  // Note there's no instance for HNil

  implicit def hsingle[K <: Symbol, T, TO]
  (implicit
    wk: Witness.Aux[K],
   h1: Mapper.Aux[FieldType[K, Mapping[T]], TO]
  ): Mapper.Aux[FieldType[K, Mapping[T]] :: HNil, TO :: HNil] = new Mapper[FieldType[K, Mapping[T]] :: HNil] {
    type Out = TO :: HNil

    override def apply(t: FieldType[K, Mapping[T]] :: HNil): Mapping[TO :: HNil] = {
      h1.apply(t.head).transform(to => to :: HNil, l => l.head)
    }
  }

  implicit def hcons[K <: Symbol, S, SO, T <: HList, TO <: HList]
  (implicit
    single: Mapper.Aux[FieldType[K, Mapping[S]], SO],
    lmapper: Mapper.Aux[T, TO]
  ): Mapper.Aux[FieldType[K, Mapping[S]] :: T, SO :: TO] = new Mapper[FieldType[K, Mapping[S]] :: T] {
    override type Out = SO :: TO

    override def apply(t: FieldType[K, Mapping[S]] :: T): Mapping[SO :: TO] = {
      val smapping = single.apply(t.head)
      val lmapping = lmapper.apply(t.tail)
      new ConsMapping(smapping, lmapping)
    }
  }

  /**
    * Case class mapping
    */
  implicit def hobjWithMappingsRecord[T, R <: HList, RO <: HList, L <: HList]
  (implicit
    gen: LabelledGeneric.Aux[T, L],
   mapper: Mapper.Aux[R, RO],
    align: Align[RO, L], // note these aligns have to come last
   align2: Align[L, RO]
  ): Aux[R, T] = new Mapper[R] {
    type Out = T

    override def apply(t: R): Mapping[T] = {
      mapper.apply(t).transform(ro => gen.from(align.apply(ro)), t => align2(gen.to(t)))
    }
  }

  /**
    * Alternatively, we could reverse the syntax and do:
    *
    * val mapping = Mapper.withMappings(('field ->> nonEmptyText) :: HNil).to[T]
    */
  def forCaseClass[T] = new {
    def withMappings[R <: HList](r: R)(implicit mapper: Mapper.Aux[R, T]) = mapper.apply(r)
  }

  class ConsMapping[H, T <: HList](hmapping: Mapping[H], tmapping: Mapping[T], val key: String = "", val constraints: Seq[Constraint[H :: T]] = Nil)
    extends Mapping[H :: T] {

    private[this] val hfield = hmapping.withPrefix(key)
    private[this] val tfields = tmapping.withPrefix(key)

    override val mappings = Seq(this) ++ hfield.mappings ++ tfields.mappings

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

    override def withPrefix(prefix: String) = {
      addPrefix(prefix).map { newKey =>
        new ConsMapping(hfield, tfields, newKey, constraints)
      }.getOrElse(this)
    }

    override def verifying(constraints: Constraint[H :: T]*) = {
      new ConsMapping(hfield, tfields, key, this.constraints ++ constraints.toSeq)
    }
  }
}