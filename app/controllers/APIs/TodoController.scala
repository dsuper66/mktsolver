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

  def solve =
    Action { request: Request[AnyContent] =>
      val body: AnyContent = request.body
      val jsonBody: Option[JsValue] = body.asJson

      // Expecting json body
      jsonBody
        // .map { json =>
        //   val subCategories =
        //     (json \ "sub-categories").as[List[Map[String, String]]]

        //   val names = subCategories.map(_("elementId"))
        //   Ok("Got: " + names)
        // }

        .map { json =>
          val subCategories =
            (json \ "elements").as[List[Map[String, String]]]

          val names = subCategories.map(_("elementId"))
          Ok("Got: " + names)
        }

        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
    }

}
