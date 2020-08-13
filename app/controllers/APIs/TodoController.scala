
package controllers.api
 
import javax.inject._
import play.api.mvc._
import play.api.libs.json._
import models.{Todo}
 
class TodoController @Inject()(
    cc: ControllerComponents
) extends AbstractController(cc) {
 
    implicit val todoFormat = Json.format[Todo]
 
    def getAll = Action {
        val todo = new Todo(1, "item 1", false)
        Ok(Json.toJson(todo))
    }
}
