package rpt.udemy.advancedScala.section5

import rpt.udemy.advancedScala.section5.HTMLSerialization.HTMLSerializer
import rpt.udemy.advancedScala.section5.TypeClasses.User

case class User(name: String, age: Int, email: String)

  object User {
    object HTMLSerialisers {
      val userSerializer: HTMLSerializer[User] = (x: User) => {
        s"<div>${x.name} ${x.age} <a href=${x.email}/></div>"
      }
      val partialUserSerializer: HTMLSerializer[User] = (x: User) => {
        s"<div>${x.name}</div>"
      }
    }
    object Equalizers {
      val fullEqualizer:Equalizer[User] = (left:User, right:User) => {
        left.name.equals(right.name) && left.email.equals(right.email)
      }
    }
    implicit val defaultSerializer: HTMLSerializer[User] = HTMLSerialisers.userSerializer
  }