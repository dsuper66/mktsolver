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

// //https://stackoverflow.com/questions/25741162/scala-reads-how-to-handle-optionmapstring-any
//   object ContractDetails {
//     implicit val readsMap =
//       Reads[Map[String, Any]](m => Reads.mapReads[Any](anyReads).reads(m))
//     implicit val reads = Json.reads[ContractDetails]
//     val anyReads = Reads[Any](m => metaValueToJsValue(m))

//     def metaValueToJsValue(m: JsValue): JsResult[Any] = {
//       m match {
//         case JsBoolean(b) => JsSuccess(b)
//         case JsNumber(n)  => JsSuccess(n)
//         case JsString(s)  => JsSuccess(s)
//         case JsArray(arr) => {
//           val list = arr.map(metaValueToJsValue)
//           JsSuccess(list)
//         }
//         case JsNull => JsSuccess(null)
//         //case x => JsFailure(x.toString())
//         case JsObject(m) => {
//           val m1 = m.map(f => (f._1, metaValueToJsValue(f._2))).toMap
//           JsSuccess(m1)
//         }
//       }
//     }
//   }

  case class ModelElement(
      elementId: String,
      elementTypeId: String,
      properties: Map[String, Any]
  )
  // properties: String)

  case class ModelElements(modelElements: Seq[ModelElement])

  object ModelElement {

    implicit val readsMap =
      Reads[Map[String, Any]](m => Reads.mapReads[Any](anyReads).reads(m))
    val anyReads = Reads[Any](m => metaValueToJsValue(m))

    def metaValueToJsValue(m: JsValue): JsResult[Any] = {
      m match {
        case JsBoolean(b) => JsSuccess(b)
        case JsNumber(n)  => JsSuccess(n)
        case JsString(s)  => JsSuccess(s)
        case JsArray(arr) => {
          val list = arr.map(metaValueToJsValue)
          JsSuccess(list)
        }
        case JsNull => JsSuccess(null)
        //case x => JsFailure(x.toString())
        case JsObject(m) => {
          val m1 = m.map(f => (f._1, metaValueToJsValue(f._2))).toMap
          JsSuccess(m1)
        }
      }
    }

    implicit val reads = Json.reads[ModelElement]
  }

  def solve2 =
    Action { request: Request[AnyContent] =>
      // response().setHeader(CACHE_CONTROL, "max-age=3600");

      val body: AnyContent = request.body
      val jsonBody: Option[JsValue] = body.asJson
                
      jsonBody
        .map { json =>
          val outString = (json \ "elements").as[Seq[ModelElement]]

          // Json.parse(s).as[Seq[ModelElement]]

          // // val elements: Map[String, Element] =
          // //   (json \ "elements").as[Map[String,Element]]
          // // // val jsonAst = json.toString // or JsonParser(source)

          // // var outString = ""
          // // for ((element, arrayOfProperties) <- elements)
          // //   outString += (s"element: $element\n")

          Ok("SCALA data:\n" + outString)
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
        .withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> "*")

    }



}
