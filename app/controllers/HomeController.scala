package controllers

import javax.inject._
import play.api._
import play.api.mvc._

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject() (val controllerComponents: ControllerComponents)
    extends BaseController {

  /**
    * Create an Action to render an HTML page.
    *
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index() =
    Action { implicit request: Request[AnyContent] =>
      Ok(views.html.index())
    }

  def save =
    Action { request: Request[AnyContent] =>
      val body: AnyContent = request.body
      val jsonBody: Option[JsValue] = body.asJson

      // Expecting json body
      jsonBody
        .map { json =>
          val subCategories =
            (json \ "sub-categories").as[List[Map[String, String]]]

          val names = subCategories.map(_("elementId"))
          Ok("Got: " + names)
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
    }

}
