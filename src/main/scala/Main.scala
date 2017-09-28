package org.tksfz.formless

import play.api.data._
import play.api.data.Forms._
import shapeless._
import shapeless.syntax.singleton._

object Main {

  case class Login(username: String, password: String)

  val form =
    ('username ->> nonEmptyText) ::
      ('password ->> nonEmptyText) :: HNil

  Mapper.get(('username ->> nonEmptyText) :: HNil)

  val mapper = Mapper.get(form)

  def main(args: Array[String]): Unit = {
    val form = Form(mapper)
    val form2 = form.bindFromRequest(Map("username" -> Seq("jane"), "password" -> Seq("123456")))
    println(form2)
  }
}
