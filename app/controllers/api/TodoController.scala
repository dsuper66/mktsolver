package controllers.api

import javax.inject._
//import models.Todo
import play.api.libs.json._
import play.api.mvc._
// import scala.util.parsing.json._
// import play.api.libs.json._
// import spray.json._
// import DefaultJsonProtocol._

import solver.MathModel.{ConstraintComp, ConstraintDef, ModelElement, SolverOption, _}

class TodoController @Inject()(
                                cc: ControllerComponents
                              ) extends AbstractController(cc) {

//  implicit val todoFormat = Json.format[Todo]

//  def getAll =
//    Action {
//      val todo = Todo(1, "item 1", false)
//      Ok(Json.toJson(todo))
//    }

  def getPropertyAsDoubleElseDefault(
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

      println("ip:" + request.remoteAddress)
      val body: AnyContent = request.body
      val jsonBody: Option[JsValue] = body.asJson

      jsonBody
        .map { json =>
          val modelElements = (json \ "elements").as[Seq[ModelElement]]

          val constraintDefs = (json \ "constraintDefs").as[Seq[ConstraintDef]]

          val constraintComps =
            (json \ "constraintComps").as[Seq[ConstraintComp]]

          val solverOptions =
            (json \ "solverOptions").as[Seq[SolverOption]]

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

              //===Define the Constraint===
              //LE or EQ
              val inEquality = constraintDef.inEquality

              //--RHS--
              //RHS from parent or value
              var rhsValue = 0.0
              //RHS from parent
              //check if RHS is a property of the parent element
              if (constraintDef.rhsProperty != "") {
                val rhsValueFromProperty = parentElement.properties.find {
                  case (name, _) => name == constraintDef.rhsProperty
                }
                if (rhsValueFromProperty.isDefined) {
                  rhsValue = rhsValueFromProperty.get._2.toString.toDouble
                }
              } else { //RHS is from specified value
                rhsValue = constraintDef.rhsValue
              }


              val constraintId = s"${constraintDef.constraintType}.${parentElement.elementId}"
              var constraintString = s"$constraintId:\n"

              //              var msgForThisConstraint = s"\n\n${parentElement.elementId} " +
//                s"has constraint: ${constraintDef.constraintType}\n with components:\n"

              //===Components of the Constraint===

              //--Var Factor from parent--
              //Check if parent element has var in the constraint components
              if (constraintDef.varType != "") {
                //Add the variable
                val variableId = createVariable(parentElement.elementId, constraintDef.varType)
                //Add its factor
                val varFactor = constraintDef.factorValue
                //TODO... add factor from property (if there ever is one)
                setVarFactor(variableId, constraintId, varFactor)
                constraintString += s" ${if (varFactor > 0) "+" else ""}$varFactor * $variableId\n"
              }

              //--Components--
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
                val childrenMatchingElementType = modelElements.filter(
                  _.elementType == constraintComp.elementType
                )
                //then check for property map from parent to child, or child to parent, or to self
                for (
                  childElement <-
                    childrenMatchingElementType
                      .filter(childMatchingType =>
                        (
                          (constraintComp.propertyMap == "all") //all bids and offers are in objective
                            || //parent matches propertyMap from child
                            //e.g. nodeBal... propertyMap is fromBus, child is dirBranch matching parent bus
                            childMatchingType.properties.exists(property =>
                              property._1 == constraintComp.propertyMap
                                && property._2 == parentElement.elementId)
                            || //or child matches propertyMap from parent
                            //e.g. power flow... propertyMap is fromBus, child is bus matching child branch
                            parentElement.properties.exists(property =>
                              property._1 == constraintComp.propertyMap
                                && property._2 == childMatchingType.elementId)
                            ||
                            constraintComp.propertyMap == "self"
                              && parentElement.elementId == childMatchingType.elementId
                          )
                      )
                ) {
                  //VarFactor for component
                  var varFactor = constraintComp.factorValue

                  //and potentially from the factorProperty of the parent or child to themselves
                  //or the parent property from the factorParentProperty of the child
                  varFactor = (varFactor
                    * getPropertyAsDoubleElseDefault(
                    childElement,
                    constraintComp.factorProperty, 1.0
                  )
                    * getPropertyAsDoubleElseDefault(
                    parentElement,
                    constraintComp.factorParentProperty, 1.0
                  )
                    * getPropertyAsDoubleElseDefault(
                    parentElement,
                    constraintDef.factorProperty, 1.0
                  ))

                  //VariableId for constraint component
                  val variableId = createVariable(childElement.elementId, constraintComp.varType)
                  //The varFactor relates the variable to the particular constraint
                  setVarFactor(variableId, constraintId, varFactor)

                  constraintString += s" ${if (varFactor > 0) "+" else ""}$varFactor * $variableId \n"
                }
              } //done components

              //Inequality RHS
              constraintString += s" $inEquality $rhsValue"

              addConstraint(
                constraintId,
                constraintDef.constraintType,
                parentElement.elementId,
                inEquality,
                rhsValue,
                constraintString)

              msg += s"$constraintString\n\n" //msgForThisConstraint
            }
          }

          //Full details
          /*
          println(
            s"modelElements:\n $modelElements\n====\n constraintDefs:\n$constraintDefs \n====\n" +
              s"constraintComps:\n$constraintComps\n====\n  $msg\n" +
              s"varFactors:\n$varFactorsString\n"
          )*/

          //Counts
          println(
            s"modelElements:${modelElements.length}\n" +
              s"constraintDefs:${constraintDefs.length}\n" +
              s"constraintComps:${constraintComps.length}\n"
          )

          //Log the ip address
          solverOptions.find(data=>data.key == "ipAddress") match {
            case Some(data) => println ("ip address:" + data.value + "\n")
            case None =>}

          Ok(solveModel)
        }
        .getOrElse {
          BadRequest("Expecting application/json request body")
        }
        .withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> "*")

    }

}
