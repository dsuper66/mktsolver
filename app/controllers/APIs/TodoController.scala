package controllers.api

import javax.inject._
import play.api.mvc._
import play.api.libs.json._
import models.{Todo}
import scala.collection.immutable.LazyList.cons
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

//A Scala Case Class is like a regular class, except it is good for modeling immutable data
  case class ModelElement(
      elementId: String,
      elementType: String,
      properties: Map[String, Any]
  )
  case class ModelElements(modelElements: Seq[ModelElement])

  case class ConstraintDef(
      constraintId: String,
      elementType: String,
      varType: String,
      inEquality: String,
      rhsProperty: String,
      rhsValue: Double
  )

  case class ConstraintComp(
      constraintId: String,
      elementType: String,
      propertyMapToParent: String,
      varType: String,
      multParentProperty: String,
      multValue: Double
  )

  case class Property( name : String , value : Any )

// For example, the type Int => String, is equivalent to
// the type Function1[Int,String] i.e. a function that takes an argument of type Int and returns a String.
// scala> val f: Function1[Int,String] = myInt => "my int: "+myInt.toString

// The final parameter list on a method can be marked implicit,
// which means the values will be taken from the context in which they are called

// An object with the same name as a class is called a companion object.
// Conversely, the class is the object’s companion class.
//A companion class or object can access the private members of its companion.
// Use a companion object for methods and values which are not specific to instances of the companion class.
//The companion object can also contain factory methods

//Play Reads converters are used to convert from a JsValue to another type.

// What's a Reads? It's just a trait that defines how a JsValue
// (the play class encapsulating JSON values) should be deserialized from JSON to some type.
// The trait only requires one method to be implemented, a reads method which
// takes in some json and returns a JsResult of some type.

// Here [A] is the type parameter for function findKth. Now what does type parameter mean?
// Type parameter tells the compiler that method findKth can take parameter of type A.
// Which is the generic type here because A can be anything.
// For example A can be Int, Double, another List -- anything.

//https://stackoverflow.com/questions/25741162/scala-reads-how-to-handle-optionmapstring-any

//mapReads... Deserializer for a `Map[String,V]`
  // implicit def mapReads[V](implicit fmtv: Reads[V]): Reads[Map[String, V]] =
  //   mapReads[String, V](JsSuccess(_))

  /**
    * Convert the JsValue into a A
    */
  // def reads(json: JsValue): JsResult[A]

  object ModelElement {

    //Reads looks for this implicit which tells it how to read [String,Any]
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

    //Json deserialiser for ModelElement (and needs to be after the above implicit for Reads)
    implicit val reads = Json.reads[ModelElement]
  }

  object ConstraintDef {
    //need Json deserializer for type
    implicit val reads = Json.reads[ConstraintDef]
  }

  object ConstraintComp {
    //need Json deserializer for type
    implicit val reads = Json.reads[ConstraintComp]
  }

  //A Play Action is a function that handles a request and generates a result to be sent to the client.
  //In Scala, a List inherits from Seq, but implements Product

  /**
    * Tries to convert the node into a T, throwing an exception if it can't. An implicit Reads[T] must be defined.
    */
  // def as[T](implicit fjs: Reads[T]): T = validate(fjs).fold(
  //   valid = identity,
  //   invalid = e => throw JsResultException(e)
  // )

  def solve2 =
    Action { request: Request[AnyContent] =>
      // response().setHeader(CACHE_CONTROL, "max-age=3600");

      val body: AnyContent = request.body
      val jsonBody: Option[JsValue] = body.asJson

      jsonBody
        .map { json =>
          val modelElements = (json \ "elements").as[Seq[ModelElement]]

          val constraintDefs = (json \ "constraintDefs").as[Seq[ConstraintDef]]

          val constraintComps =
            (json \ "constraintComps").as[Seq[ConstraintComp]]

          var msg = ""

          //Constraint Defs
          for (constraintDef <- constraintDefs) {
            //Get the parent elements that match the Constraint Def parent type
            //e.g. for node balance, do each bus
            for (
              parentElement <-
                modelElements.filter(_.elementType == constraintDef.elementType)
            ) {
              msg += s"\n\n ${parentElement.elementId} has constraint: ${constraintDef.constraintId} with components:"

              //Is the parent element part of the constraint
              if (constraintDef.varType != "") {
                msg += s" 1* ${parentElement.elementId}.${constraintDef.varType} "
              }

              //Get the constraint components
              for (
                constraintComp <- constraintComps.filter(
                  _.constraintId == constraintDef.constraintId
                )
              ) {
                //Get component elements where their property named propertyMapToParent
                //matches the Id of the constraint def parent
                //Case classes are especially useful for pattern matching...
                for (
                  childElement <-
                    modelElements
                      .filter(
                        _.elementType == constraintComp.elementType
                      )
                      .filter(_.properties.filter {
                        case (name, value) =>
                          (
                            name,
                            value
                          ) == (constraintComp.propertyMapToParent, parentElement.elementId)
                      }.headOption != None)
                ) {
                  msg += constraintComp.multValue + "*"
                  msg += childElement.elementId + "." + constraintComp.varType + " "
                }
              }//done components

              //LE or EQ
              msg += s"${constraintDef.inEquality}"

              //RHS              
              //check if RHS is a property of the parent element
              if (constraintDef.rhsProperty != "") {
                val rhsValueFromProperty = parentElement.properties.filter{
                  case(name,value) => name == constraintDef.rhsProperty}.headOption
                if (rhsValueFromProperty != None) {
                  msg += s" ${rhsValueFromProperty}"
                }
              }
              else { //RHS from value
                msg += s" ${constraintDef.rhsValue}"
              }
            }
          }
          // Json.parse(s).as[Seq[ModelElement]]

          // // val elements: Map[String, Element] =
          // //   (json \ "elements").as[Map[String,Element]]
          // // // val jsonAst = json.toString // or JsonParser(source)

          // // var outString = ""
          // // for ((element, arrayOfProperties) <- elements)
          // //   outString += (s"element: $element\n")

          Ok(
            "SCALA data:\n" + modelElements + "\r\n"
              + "====\n" + constraintDefs + "\n====\n" + constraintComps + "\n====\n" + msg
          )
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
        .withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> "*")

    }

}
