package com.iterable.formless

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.string.Url
import play.api.data.Forms._
import play.api.data._
import shapeless._
import shapeless.syntax.singleton._

object Main {

  case class Login(username: String, password: String)

  case class CreateAccount(
    name: String Refined NonEmpty,
    numUsers: Int Refined Positive,
    paid: Boolean,
    website: String Refined Url
  )

  val record =
      ('password ->> nonEmptyText) :: ('username ->> nonEmptyText) :: HNil

  MkMapping.get(('username ->> nonEmptyText) :: HNil)

  val mapper = MkMapping.get(record)

  val request = Map("username" -> Seq("jane"), "password" -> Seq("123456"))

  def main(args: Array[String]): Unit = {
    val form = Form(mapper)
    val form2 = form.bindFromRequest(request)
    println(form2)

    two()

    defaults()
  }

  def two(): Unit = {
    val mapper = MkMapping.forCaseClass[Login].withMappings(record)
    val form = Form(mapper)

    form("username")
    //form('username)
    println(form.bindFromRequest(request))
  }

  val safeForm = MkMapping.forCaseClass[Login].safeForm(record)

  def three(): Unit = {
    val newSafeForm = safeForm.bindFromRequest(request)
    view(newSafeForm)
  }


  def view(form: Main.safeForm.Repr) = {
    form('username)
  }

  def defaults() = {
    val mapping = MkMapping.forCaseClass[Login].withDefaultsAndMappings(DefaultsWithNonEmptyText, 'password ->> text :: HNil)
    println(mapping.bind(Map("username" -> "", "password" -> "pw")))
  }

}
