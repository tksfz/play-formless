package org.tksfz.formless

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraint
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.Align
import shapeless.ops.record.Selector


class SafeForm[RO <: HList, T] private(form: Form[T]) {

  type Repr = SafeForm[RO, T]

  // TODO: get this to work with Witness.Aux
  def apply(k: Witness)
  (implicit
    kInRO: Selector[RO, k.T],
   kSubSymbol: k.T <:< Symbol): Field = {
    val field = k.value.name
    form.apply(field)
  }

def bindFromRequest(data: Map[String, Seq[String]]): SafeForm[RO, T] = {
  val newForm = form.bindFromRequest(data)
  new SafeForm(newForm)
}

  def bindFromRequest()(implicit request: play.api.mvc.Request[_]) = {
    val newForm = form.bindFromRequest()(request)
    new SafeForm(newForm)
  }
}

object SafeForm {
  def apply[RO <: HList, T, L <: HList](premapping: Mapping[RO], data: Map[String, String], errors: Seq[FormError], value: Option[T])(implicit
 gen: LabelledGeneric.Aux[T, L],
 align: Align[RO, L], // note these aligns have to come last
 align2: Align[L, RO]
): SafeForm[RO, T] = {
    val mapping: Mapping[T] = premapping.transform(ro => gen.from(align.apply(ro)), t => align2(gen.to(t)))
    val form = Form(mapping, data, errors, value)
    new SafeForm(form)
  }
}

/**
  * Type class supporting creation of Mappings from specifications
  */
trait MkMapping[T] {
  type Out
  def apply(t: T): Mapping[Out]
}

object MkMapping {

  type Aux[T, Out0] = MkMapping[T] { type Out = Out0 }

  def get[L <: HList](l: L)(implicit mapper: MkMapping[L]) = mapper.apply(l)

  /**
    * Instance for mappings
    */
  implicit def mkMapping[T] = new MkMapping[Mapping[T]] {
    override type Out = T
    override def apply(t: Mapping[T]): Mapping[T] = t
  }

  /**
    * Instance for record elements that supply a Mapping with a Symbol singleton key:
    *
    *     'foo ->> nonEmptyText => 'foo ->> String
    */
  implicit def kvMkMapping[K <: Symbol, T]
  (implicit wk: Witness.Aux[K]): Aux[FieldType[K, Mapping[T]], FieldType[K, T]] = new MkMapping[FieldType[K, Mapping[T]]] {
    override type Out = FieldType[K, T]
    override def apply(t: FieldType[K, Mapping[T]]): Mapping[FieldType[K, T]] = {
      // Since FieldType[K, V] == V tagged with K, the transformation is trivial
      t.withPrefix(wk.value.name).transform(t => field[K](t), identity)
    }
  }

  /**
    * Instances for HLists (both ordinary HLists and records, by the kv instances above)
    */

  // Note there's no instance for HNil

  implicit def hsingleMkMapping[T, TO]
  (implicit
    tMkMapping: MkMapping.Aux[T, TO]
  ): Aux[T :: HNil, TO :: HNil] = new MkMapping[T :: HNil] {
    override type Out = TO :: HNil

    override def apply(l: T :: HNil): Mapping[TO :: HNil] = {
      tMkMapping.apply(l.head).transform(to => to :: HNil, l => l.head)
    }
  }

  implicit def hconsMkMapping[H, HO, T <: HList, TO <: HList]
  (implicit
    hMkMapping: MkMapping.Aux[H, HO],
    tMkMapping: MkMapping.Aux[T, TO]
  ): Aux[H :: T, HO :: TO] = new MkMapping[H :: T] {
    override type Out = HO :: TO

    override def apply(t: H :: T): Mapping[HO :: TO] = {
      val hmapping = hMkMapping.apply(t.head)
      val lmapping = tMkMapping.apply(t.tail)
      new ConsMapping(hmapping, lmapping)
    }
  }

  /**
    * Case class mapping
    */
  implicit def hobjWithMappingsRecord[T, R <: HList, RO <: HList, L <: HList]
  (implicit
   gen: LabelledGeneric.Aux[T, L],
   mkMapping: MkMapping.Aux[R, RO],
   align: Align[RO, L], // note these aligns have to come last
   align2: Align[L, RO]
  ): Aux[R, T] = new MkMapping[R] {
    override type Out = T

    override def apply(r: R): Mapping[T] = {
      mkMapping.apply(r).transform(ro => gen.from(align.apply(ro)), t => align2(gen.to(t)))
    }
  }

  /**
    * Alternatively, we could reverse the syntax and do:
    *
    * val mapping = Mapper.withMappings(('field ->> nonEmptyText) :: HNil).to[T]
    */
  def forCaseClass[T] = new {
    def withMappings[R <: HList](r: R)(implicit mapper: MkMapping.Aux[R, T]): Mapping[T] = mapper.apply(r)


    def getWrapper[L <: HList, R <: HList, RO <: HList](r: R)
  (implicit
   gen: LabelledGeneric.Aux[T, L],
   mkMapping: MkMapping.Aux[R, RO],
   align: Align[RO, L], // note these aligns have to come last
   align2: Align[L, RO]
  ): SafeForm[RO, T] = {
      val premapping = mkMapping.apply(r)
      SafeForm.apply(premapping, Map(), Seq(), None)
  }

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