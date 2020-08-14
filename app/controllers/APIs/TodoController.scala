package controllers.api

import javax.inject._
import play.api.mvc._
import play.api.libs.json._
import models.{Todo}

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
            
              val elements:Map[String, String] =
            (json \ "elements").as[Map[String, String]]

            var outString = ""
            for ((element,bus) <- elements) outString += (s"element: $element, bus: $bus\n")

            Ok("Got:\n" + outString)
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
    }

}
