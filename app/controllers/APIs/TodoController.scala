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

class TodoController @Inject()(
                                cc: ControllerComponents
                              ) extends AbstractController(cc) {

  implicit val todoFormat = Json.format[Todo]

  def getAll =
    Action {
      val todo = Todo(1, "item 1", false)
      Ok(Json.toJson(todo))
    }

  def getPropertyAsDoubleOrOne(
                                element: ModelElement,
                                propertyType: String,
                              default: Double
                              ): Double = {

    val matchingProperty = element.properties.find(property => property._1 == propertyType)

    if (matchingProperty.isDefined) {
      matchingProperty.get._2.toString.toDouble
    } else {
      default
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
          resetMathModel()

          //Constraint Defs
          for (constraintDef <- constraintDefs) {
            //Get the parent elements that match the ConstraintDef elementType
            //e.g. for node balance, do each bus
            for (
              parentElement <-
                modelElements.filter(_.elementType == constraintDef.elementType)
            ) {

              //Constraint
              //LE or EQ
              val inEquality = constraintDef.inEquality
              //RHS
              var rhsValue = 0.0
              //RHS
              //check if RHS is a property of the parent element
              if (constraintDef.rhsProperty != "") {
                val rhsValueFromProperty = parentElement.properties.find {
                  case (name, _) => name == constraintDef.rhsProperty
                }
                if (rhsValueFromProperty.isDefined) {
                  rhsValue = rhsValueFromProperty.get._2.toString.toDouble
                  //                  msgForThisConstraint += s" ${rhsValueFromProperty.get._2}"
                }
                //                else { //Error if the RHS property is missing (constraint not created)
                //                  msgForThisConstraint =
                //                    s"\n ERROR ${constraintDef.constraintType} " +
                //                      s"for ${parentElement.elementId} has RHS ${constraintDef.rhsProperty} but property not found"
                //                }
              } else { //RHS is from specified value
                rhsValue = constraintDef.rhsValue
                //                msgForThisConstraint += s" ${constraintDef.rhsValue}"
              }

              //Add the constraint entry
              val constraintId = addConstraint(constraintDef.constraintType, parentElement.elementId, inEquality, rhsValue)

              var msgForThisConstraint = s"\n${parentElement.elementId} " +
                s"has constraint: ${constraintDef.constraintType}\nwith components:\n"

              //Var Id and Factor... Does the parent element have a var in the constraint
              if (constraintDef.varType != "") {
                //                val varId = s"${parentElement.elementId}.${constraintDef.varType}"
                val varId = addVar(parentElement.elementId, constraintDef.varType)
                val varFactor = 1.0
                //                addVarIfNew(varId)
                setVarFactor(varId, constraintId, varFactor)
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
                        (//parent matches propertyMap from child
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
                  //VarFactor for component
                  var varFactor = constraintComp.multValue

                  //The varFactor is also from the multProperty of the parent or child
                  //or the multParentProperty of the child
                  varFactor = (varFactor
                    * getPropertyAsDoubleOrOne(
                    childElement,
                    constraintComp.multProperty,1.0
                  )
                    * getPropertyAsDoubleOrOne(
                    parentElement,
                    constraintComp.multParentProperty, 1.0
                  )
                    * getPropertyAsDoubleOrOne(
                    parentElement,
                    constraintDef.multProperty, 1.0
                  ))

                  //Var Id for component
                  val varId = addVar(childElement.elementId, constraintComp.varType)
                  setVarFactor(varId, constraintId, varFactor)

                  msgForThisConstraint += s" $varFactor * $varId \n"
                }
              } //done components

              //Inequality RHS
              msgForThisConstraint += s" $inEquality $rhsValue"

              msg += msgForThisConstraint
            }
          }

          Ok(
            s"SCALA data:\n $modelElements\n====\n constraintDefs:\n$constraintDefs \n====\n" +
              s"constraintComps:\n$constraintComps\n====\n  $msg\n" +
              s"varString:\n$varsString\n\n$varFactorsString\n\n$solveModel"
          )
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
        .withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> "*")

    }

}
