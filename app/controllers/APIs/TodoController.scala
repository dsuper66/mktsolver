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

import solver.MathModel.ModelElement
import solver.MathModel.ModelElements
import solver.MathModel.ConstraintDef
import solver.MathModel.ConstraintComp
import solver.MathModel._

class TodoController @Inject() (
    cc: ControllerComponents
) extends AbstractController(cc) {

  implicit val todoFormat = Json.format[Todo]

  def getAll =
    Action {
      val todo = new Todo(1, "item 1", false)
      Ok(Json.toJson(todo))
    }

  // case class Element(elementId: String, bus: String)

  // case class ModelElement(
  //     elementId: String,
  //     elementType: String,
  //     properties: Map[String, Any]
  // )

  // case class ModelElements(modelElements: Seq[ModelElement])

  // case class ConstraintDef(
  //     constraintId: String,
  //     elementType: String,
  //     varType: String,
  //     inEquality: String,
  //     rhsProperty: String,
  //     rhsValue: Double,
  //     multProperty: String
  // )

  // case class ConstraintComp(
  //     constraintId: String,
  //     elementType: String,
  //     propertyMap: String,
  //     varType: String,
  //     multParentProperty: String,
  //     multValue: Double,
  //     multProperty: String
  // )

  // case class Property(name: String, value: Any)

  // object ModelElement {

  //   //Reads looks for this implicit which tells it how to read [String,Any]
  //   implicit val readsMap =
  //     Reads[Map[String, Any]](m => Reads.mapReads[Any](anyReads).reads(m))

  //   val anyReads = Reads[Any](m => metaValueToJsValue(m))

  //   def metaValueToJsValue(m: JsValue): JsResult[Any] = {
  //     m match {
  //       case JsBoolean(b) => JsSuccess(b)
  //       case JsNumber(n)  => JsSuccess(n)
  //       case JsString(s)  => JsSuccess(s)
  //       case JsArray(arr) => {
  //         val list = arr.map(metaValueToJsValue)
  //         JsSuccess(list)
  //       }
  //       case JsNull => JsSuccess(null)
  //       //case x => JsFailure(x.toString())
  //       case JsObject(m) => {
  //         val m1 = m.map(f => (f._1, metaValueToJsValue(f._2))).toMap
  //         JsSuccess(m1)
  //       }
  //     }
  //   }

  //   //Json deserialiser for ModelElement (and needs to be after the above implicit for Reads)
  //   implicit val reads = Json.reads[ModelElement]
  // }

  // object ConstraintDef {
  //   //need Json deserializer for type
  //   implicit val reads = Json.reads[ConstraintDef]
  // }

  // object ConstraintComp {
  //   //need Json deserializer for type
  //   implicit val reads = Json.reads[ConstraintComp]
  // }

  //A Play Action is a function that handles a request and generates a result to be sent to the client.
  //In Scala, a List inherits from Seq, but implements Product

  /**
    * Tries to convert the node into a T, throwing an exception if it can't. An implicit Reads[T] must be defined.
    */
  // def as[T](implicit fjs: Reads[T]): T = validate(fjs).fold(
  //   valid = identity,
  //   invalid = e => throw JsResultException(e)
  // )

  def getPropertyAsDoubleOrOne(
      element: ModelElement,
      propertyType: String
  ): Double = {

    val matchingProperty = element.properties
      .filter(property => property._1 == propertyType)
      .headOption

    if (matchingProperty != None) {
      matchingProperty.get._2.toString().toDouble
    } else {
      1.0
    }
  }

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
            //Get the parent elements that match the ConstraintDef elementType
            //e.g. for node balance, do each bus
            for (
              parentElement <-
                modelElements.filter(_.elementType == constraintDef.elementType)
            ) {

              //Constraint Id
              val constraintId =
                s"${constraintDef.constraintId}.${parentElement.elementId}"
              addConstraintIfNew(constraintId)
              

              var msgForThisConstraint = (s"\n${parentElement.elementId} " +
                s"has constraint: ${constraintDef.constraintId}\nwith components:\n")

              //Variable Id... Does the parent element have a var in the constraint
              if (constraintDef.varType != "") {
                val varId = s"${parentElement.elementId}.${constraintDef.varType}"
                addVarIfNew(varId)
                msgForThisConstraint += s" +1* $varId\n"
              }

              //Get the constraint components
              for ( //Get components where the constraint Id property matches
                constraintComp <- constraintComps.filter(
                  _.constraintId == constraintDef.constraintId
                )
              ) {
                //Get component elements where their elementType matches AND their property
                //as specified by propertyMap matches the constraintDef parent element
                //(case classes are especially useful for pattern matching...)

                //elements where elementType matches constraint component
                val childElementsMatchingType = modelElements.filter(
                  _.elementType == constraintComp.elementType
                )
                //then check for property map from parent to child or child to parent
                for (
                  childElement <-
                    childElementsMatchingType
                      .filter(childElementMatching =>
                        ( //parent matches propertyMap from child
                          (constraintComp.propertyMap == "all") //all bids and offers are in objective
                            ||
                              (childElementMatching.properties
                                .filter(property =>
                                  (property._1 == constraintComp.propertyMap
                                    && property._2 == parentElement.elementId)
                                )
                                .headOption != None)
                            || //or child matches propertyMap from parent
                              (parentElement.properties
                                .filter(property =>
                                  (property._1 == constraintComp.propertyMap
                                    && property._2 == childElementMatching.elementId)
                                )
                                .headOption != None)
                        )
                      )
                ) {
                  //The multiplier for the component
                  var multiplier = constraintComp.multValue

                  //The multiplier is also from the multProperty of the parent or child
                  //or the multParentProperty of the child
                  multiplier = (multiplier
                    * getPropertyAsDoubleOrOne(
                      childElement,
                      constraintComp.multProperty
                    )
                    * getPropertyAsDoubleOrOne(
                      parentElement,
                      constraintComp.multParentProperty
                    )
                    * getPropertyAsDoubleOrOne(
                      parentElement,
                      constraintDef.multProperty
                    ))
                  
                  //Variable Id
                  val varId = s"${childElement.elementId}.${constraintComp.varType}"
                  addVarIfNew(varId)
                  msgForThisConstraint += s" $multiplier * $varId \n"
                }
              } //done components

              //LE or EQ
              val inEquality = constraintDef.inEquality
              msgForThisConstraint += s" $inEquality"

              //RHS
              //check if RHS is a property of the parent element
              if (constraintDef.rhsProperty != "") {
                val rhsValueFromProperty = parentElement.properties.filter {
                  case (name, value) => name == constraintDef.rhsProperty
                }.headOption

                if (rhsValueFromProperty != None) {
                  msgForThisConstraint += s" ${rhsValueFromProperty.get._2}"
                } else { //Error if the RHS property is missing (constraint not created)
                  msgForThisConstraint =
                    (s"\n ERROR ${constraintDef.constraintId} " +
                      s"for ${parentElement.elementId} has RHS ${constraintDef.rhsProperty} but property not found")
                }
              } else { //RHS from value
                msgForThisConstraint += s" ${constraintDef.rhsValue}"
              }

              msg += msgForThisConstraint
            }
          }

          Ok(
            (s"SCALA data:\n $modelElements\n====\n $constraintDefs \n====\n$constraintComps\n====\n  " +
              s"$msg\n\n$constraintsString\n\n$varsString")
          )
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
        .withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> "*")

    }

}
