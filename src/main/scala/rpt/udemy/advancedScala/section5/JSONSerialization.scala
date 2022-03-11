package rpt.udemy.advancedScala.section5

import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.util.Date

object JSONSerialization extends App {

  /**
    Social Network with Users, posts, feeds, etc.
  */

  case class User(name: String, age: Int, email: String)

  object User {
    val NAME="name"
    val AGE="age"
    val EMAIL="email"
    implicit val jsonifier: Jsonifier[User] = (user:User) => {
      JSONObject(
        values = Map(
          NAME -> JSONString(user.name),
          AGE -> JSONNumber(user.age),
          EMAIL -> JSONString(user.email)
        )
      )
    }
    implicit val jsonSerializer:JsonSerializer[User] = (user:User) => user.jsonify.stringify
  }

  case class Post(content: String, createdAt: Date)

  object Post {
    val CONTENT="content"
    val CREATED_AT="createdAt"
    implicit val jsonifier: Jsonifier[Post] = (post:Post) => {
      JSONObject(
        Map(
          CONTENT -> JSONString(post.content),
          CREATED_AT -> JSONDate(post.createdAt)
        )
      )
    }
    implicit val jsonSerializer: JsonSerializer[Post] = (post:Post) => post.jsonify.stringify
  }

  case class Feed(user: User, posts: List[Post])

  object Feed {
    val USER="user"
    val POSTS="posts"
    implicit val jsonifier: Jsonifier[Feed] = (feed:Feed) => {
      JSONObject(
        Map(
          USER -> feed.user.jsonify,
          POSTS -> JSONArray(feed.posts.map(_.jsonify))
        )
      )
    }
    implicit val jsonSerializer: JsonSerializer[Feed] = (feed:Feed) => feed.jsonify.stringify

  }

  /*
    We want to serialize them using TypeClass pattern.
    Steps:
      1. Intermediate data types for: Int, Date, List, String
      2. TypeClass for conversion from case classes to intermediate types
      3. serialize to JSON
  */

  // 1. intermediate data type
  sealed trait JSONValue {
    def stringify: String
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

  final case class JSONDate(value:Date) extends JSONValue {
    val formatter = new SimpleDateFormat("%y-%M-%d")
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
  trait Jsonifier[T] {
    def jsonify(value: T):JSONValue
  }

  object Jsonifier {
    def apply[T: Jsonifier](value:T):JSONValue = {
      val jsonifier: Jsonifier[T] = implicitly[Jsonifier[T]]
      jsonifier.jsonify(value)
    }
  }

  trait JsonSerializer[T] {
    def serialize(value:T):String
  }

  object JsonSerializer {
    def apply[T: JsonSerializer](value:T):String = {
      val serializer: JsonSerializer[T] = implicitly[JsonSerializer[T]]
      serializer.serialize(value)
    }
  }

  implicit class JsonEnrichment[T](value:T) {
    def toJson(implicit serializer:JsonSerializer[T]):String = JsonSerializer[T](value)
    def jsonify(implicit jsonifier: Jsonifier[T]):JSONValue = Jsonifier[T](value)
  }

  // Test
  val rafa = User("rafa", 32, "rpt@email.com")
  val post1 = Post("Hello World!", new Date(2022,1,1))
  val post2 = Post("Good Night World!", new Date(2022,12,31))
  val feed = Feed(user = rafa, posts = List(post1, post2))

  println {
    feed.toJson
  }

}
