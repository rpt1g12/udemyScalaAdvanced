package rpt.udemy.advancedScala.section5

import java.text.SimpleDateFormat
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object JSONSerialization extends App {

  /**
    Social Network with Users, posts, feeds, etc.
  */

  case class User(name: String, age: Int, email: String)

  object User {
    import JSONConverter.implicits._
    implicit object UserConverter extends JSONConverter[User] {
      override def convert(value: User): JSONValue = {
        val converter: JSONConverter[Product] = implicitly[JSONConverter[Product]]
        converter.convert(value)
      }
    }
  }

  case class Post(content: String, createdAt: LocalDate)

  object Post {
    import JSONConverter.implicits._
    implicit object PostConverter extends JSONConverter[Post] {
      override def convert(value: Post): JSONValue = {
        val converter: JSONConverter[Product] = implicitly[JSONConverter[Product]]
        converter.convert(value)
      }
    }
  }

  case class Feed(user: User, posts: List[Post])

  object Feed {
    import JSONConverter.implicits._
    implicit object FeedConverter extends JSONConverter[Feed] {
      override def convert(value: Feed): JSONValue = {
        val converter: JSONConverter[Product] = implicitly[JSONConverter[Product]]
        converter.convert(value)
      }
    }
  }

  /*
    We want to convert them using TypeClass pattern.
    Steps:
      1. Intermediate data types for: Int, Date, List, String
      2. TypeClass for conversion from case classes to intermediate types
      3. convert to JSON
  */

  // 1. intermediate data type
  sealed trait JSONValue {
    def stringify: String
    override def toString: String = stringify
  }

  final case class JSONString(value:String) extends JSONValue {
    override def stringify: String = s"\"$value\""
  }

  final case class JSONNumber(value:Int) extends JSONValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(values:List[JSONValue]) extends JSONValue {
    override def stringify: String = values.map(_.stringify).mkString("[",", ","]")
  }

  final case class JSONDate(value:LocalDate) extends JSONValue {
    val formatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE
    override def stringify: String = s"Date(${formatter.format(value)})"
  }

  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {
    override def stringify: String = values.map {
        case (key, value) => s"\"$key\": ${value.stringify}"
      }.mkString("{",", ","}")
  }

  // test
  val jsonObject = JSONObject(
    Map(
      "name" -> JSONString("Name"),
      "age" -> JSONNumber(1),
      "fav_colours" -> JSONArray(List(JSONString("blue"),JSONString("red")))
    )
  )

  println(jsonObject.stringify)

  // 2. Type Classes
  trait JSONConverter[T] {
    def convert(value:T):JSONValue
  }

  object JSONConverter {
    def apply[T: JSONConverter](value:T):JSONValue = {
      val serializer: JSONConverter[T] = implicitly[JSONConverter[T]]
      serializer.convert(value)
    }
    object implicits {
      implicit object StringConverter extends JSONConverter[String] {
        override def convert(value: String): JSONValue = JSONString(value)
      }
      implicit object IntConverter extends JSONConverter[Int] {
        override def convert(value: Int): JSONValue = JSONNumber(value)
      }
      implicit object DateConverter extends JSONConverter[LocalDate] {
        override def convert(value: LocalDate): JSONValue = JSONDate(value)
      }
      implicit object ListConverter extends JSONConverter[List[Any]] {
        override def convert(value: List[Any]): JSONValue = {
          JSONArray(
            value.map {
              case field:String => field.toJson
              case field:Int => field.toJson
              case field:LocalDate => field.toJson
              case field:List[Any] => field.toJson
              case field:Product => field.toJson
            }
          )
        }
      }
      implicit object ProductConverter extends JSONConverter[Product] {
        override def convert(value: Product): JSONValue = {
          JSONObject(
            Seq.range(0,value.productArity).map {
              index => value.productElementName(index) -> value.productElement(index)
            }.map {
              case (key:String, field:String) => key -> field.toJson
              case (key:String, field:Int) => key -> field.toJson
              case (key:String, field:LocalDate) => key -> field.toJson
              case (key:String, field:List[Any]) => key -> field.toJson
              case (key:String, field:Product) => key -> field.toJson
            }.toMap
          )
        }
      }
    }
  }

  implicit class JsonEnrichment[T](value:T) {
    def toJson(implicit serializer:JSONConverter[T]):JSONValue = JSONConverter[T](value)
  }

  // Test
  val rafa = User("rafa", 32, "rpt@email.com")
  val post1 = Post("Hello World!", LocalDate.of(2022,1,1))
  val post2 = Post("Good Night World!", LocalDate.of(2022,12,31))
  val feed = Feed(user = rafa, posts = List(post1, post2))



  println {
    feed.toJson
  }

}
