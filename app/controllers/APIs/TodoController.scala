package controllers.api

import javax.inject._
import play.api.mvc._
import play.api.libs.json._
import models.{Todo}
// import scala.util.parsing.json._
// import play.api.libs.json._
// import spray.json._
// import DefaultJsonProtocol._

class TodoController @Inject() (
    cc: ControllerComponents
) extends AbstractController(cc) {

  implicit val todoFormat = Json.format[Todo]

  def getAll =
    Action {
      val todo = new Todo(1, "item 1", false)
      Ok(Json.toJson(todo))
    }

  case class Element(elementId: String, bus: String)

  def solve =
    Action { request: Request[AnyContent] =>
      val body: AnyContent = request.body
      val jsonBody: Option[JsValue] = body.asJson

      // Expecting json body
      //   jsonBody
      // .map { json =>
      //   val subCategories =
      //     (json \ "sub-categories").as[List[Map[String, String]]]

      //   val names = subCategories.map(_("elementId"))
      //   Ok("Got: " + names)
      // }

      jsonBody
        .map { json =>
          // val elementsToBus = Json.parse(json).as[Element]
          // val map:Map[String, String] = json.asInstanceOf[Map[String, String]]
          // var elementsToBus = jValue.extract[Element];

          // val elements:List[Map[String, String]] =
          // (json \ "elements").as[List[Map[String, String]]]

          val elements: Map[String, String] =
            (json \ "bus1").as[Map[String, String]]

          var outString = ""
          for ((element, bus) <- elements)
            outString += (s"element: $element, bus1: $bus\n")

          Ok("Got bus1:\n" + outString)
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
    }

  def solve2 =
    Action { request: Request[AnyContent] =>
      val body: AnyContent = request.body
      val jsonBody: Option[JsValue] = body.asJson

      jsonBody
        .map { json =>
          val jsonAst = json.toString // or JsonParser(source)


          Ok("Got data:\n" + jsonAst)
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
    }

}
