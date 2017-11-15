package com.iterable.formless

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.string.MatchesRegex
import play.api.data.Forms._
import play.api.data._
import shapeless._
import shapeless.syntax.singleton._
import play.api.data.format.Formats._    // This is required

object Main {

  case class Login(username: String, password: String)

  case class CreateAccount(
    name: String Refined NonEmpty,
    numUsers: Int Refined Positive,
    paid: Boolean,
    zip: String Refined MatchesRegex[Witness.`"""\\d{5}"""`.T]
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

    refinedGood()
    refinedBad()
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

  def refinedGood(): Unit = {
    val mapping = MkMapping.forCaseClass[CreateAccount].withDefaults(DefaultsWithRefined)
    val values = Map(
      "name" -> "foo",
      "numUsers" -> "100",
      "paid" -> "true",
      "zip" -> "94107"
    )

    println(mapping.bind(values))
  }

  def refinedBad(): Unit = {
    val mapping = MkMapping.forCaseClass[CreateAccount].withDefaults(DefaultsWithRefined)
    val values = Map(
      "name" -> "",
      "numUsers" -> "-1",
      "paid" -> "true",
      "zip" -> "941XX"
    )

    println(mapping.bind(values))
  }

}
