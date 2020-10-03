package controllers.api

import javax.inject._
import models.Todo
import play.api.libs.json._
import play.api.mvc._
// import scala.util.parsing.json._
// import play.api.libs.json._
// import spray.json._
// import DefaultJsonProtocol._

import solver.MathModel.{ConstraintComp, ConstraintDef, ModelElement, _}

class TodoController @Inject() (
    cc: ControllerComponents
) extends AbstractController(cc) {

  implicit val todoFormat = Json.format[Todo]

  def getAll =
    Action {
      val todo = new Todo(1, "item 1", false)
      Ok(Json.toJson(todo))
    }

  def getPropertyAsDoubleOrOne(
      element: ModelElement,
      propertyType: String
  ): Double = {

    val matchingProperty = element.properties.find(property => property._1 == propertyType)

    if (matchingProperty.isDefined) {
      matchingProperty.get._2.toString.toDouble
    } else {
      1.0
    }
  }

  def solve2: Action[AnyContent] =
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
                s"${constraintDef.constraintType}.${parentElement.elementId}"
              addConstraintIfNew(constraintId)
              

              var msgForThisConstraint = s"\n${parentElement.elementId} " +
                s"has constraint: ${constraintDef.constraintType}\nwith components:\n"

              //Var Id... Does the parent element have a var in the constraint
              if (constraintDef.varType != "") {
                val varId = s"${parentElement.elementId}.${constraintDef.varType}"
                val varFactor = 1.0
                addVarIfNew(varId)
                setVarFactor(varId,constraintId,varFactor)
                msgForThisConstraint += s" $varFactor * $varId\n"
              }

              //Get the constraint components
              for ( //Get components where the constraint Id property matches
                constraintComp <- constraintComps.filter(
                  _.constraintType == constraintDef.constraintType
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
                              childElementMatching.properties.exists(property =>
                                (property._1 == constraintComp.propertyMap
                                  && property._2 == parentElement.elementId))
                            || //or child matches propertyMap from parent
                              parentElement.properties.exists(property =>
                                property._1 == constraintComp.propertyMap
                                  && property._2 == childElementMatching.elementId)
                        )
                      )
                ) {
                  //The varFactor for the component
                  var varFactor = constraintComp.multValue

                  //The varFactor is also from the multProperty of the parent or child
                  //or the multParentProperty of the child
                  varFactor = (varFactor
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
                  
                  //Var Id
                  val varId = s"${childElement.elementId}.${constraintComp.varType}"
                  addVarIfNew(varId)
                  setVarFactor(varId,constraintId,varFactor)

                  msgForThisConstraint += s" $varFactor * $varId \n"
                }
              } //done components

              //LE or EQ
              val inEquality = constraintDef.inEquality
              msgForThisConstraint += s" $inEquality"

              //RHS
              //check if RHS is a property of the parent element
              if (constraintDef.rhsProperty != "") {
                val rhsValueFromProperty = parentElement.properties.find {
                  case (name, value) => name == constraintDef.rhsProperty
                }

                if (rhsValueFromProperty.isDefined) {
                  msgForThisConstraint += s" ${rhsValueFromProperty.get._2}"
                } else { //Error if the RHS property is missing (constraint not created)
                  msgForThisConstraint =
                    s"\n ERROR ${constraintDef.constraintType} " +
                      s"for ${parentElement.elementId} has RHS ${constraintDef.rhsProperty} but property not found"
                }
              } else { //RHS from value
                msgForThisConstraint += s" ${constraintDef.rhsValue}"
              }

              msg += msgForThisConstraint
            }
          }

          Ok(
            s"SCALA data:\n $modelElements\n====\n $constraintDefs \n====\n$constraintComps\n====\n  " +
              s"$msg\n\n$constraintsString\n\n$varsString\n\n$varFactorsString"
          )
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
        .withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> "*")

    }

}
