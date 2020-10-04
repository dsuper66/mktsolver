package solver

import play.api.libs.json._

//MathModel Module
object MathModel {

  case class Element(elementId: String, bus: String)

  case class ModelElement(
                           elementId: String,
                           elementType: String,
                           properties: Map[String, Any]
                         )

  case class ModelElements(modelElements: Seq[ModelElement])

  case class ConstraintDef(
                            constraintType: String,
                            elementType: String,
                            varType: String,
                            inEquality: String,
                            rhsProperty: String,
                            rhsValue: Double,
                            multProperty: String
                          )

  case class ConstraintComp(
                             constraintType: String,
                             elementType: String,
                             propertyMap: String,
                             varType: String,
                             multParentProperty: String,
                             multValue: Double,
                             multProperty: String
                           )

  // case class Property(name: String, value: Any)

  object ModelElement {
    //Reads looks for this implicit which tells it how to read [String,Any]
    implicit val readsMap =
      Reads[Map[String, Any]](m => Reads.mapReads[Any](anyReads).reads(m))

    val anyReads = Reads[Any](m => metaValueToJsValue(m))

    def metaValueToJsValue(m: JsValue): JsResult[Any] = {
      m match {
        case JsBoolean(b) => JsSuccess(b)
        case JsNumber(n) => JsSuccess(n)
        case JsString(s) => JsSuccess(s)
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

    //Json deserializer for ModelElement (and needs to be after the above implicit for Reads)
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

  //Solver Definitions
  case class VarFactor(
                        varId: String,
                        constraintId: String,
                        value: Double
                      )

  case class Constraint(
                         constraintId: String,
                         constraintType: String,
                         elementId: String,
                         inequality: String,
                         rhsValue: Double
                       )

  case class Variable(
                       varId: String,
                       varType: String,
                       elementId: String
                     )

  //Data
  var constraintIds: Seq[String] = Seq()
  var varIds: Seq[String] = Seq()

  var varFactorRows: Seq[Seq[Double]] = Seq()
  var varFactors: Seq[VarFactor] = Seq()
  var constraints: Seq[Constraint] = Seq()
  var variables: Seq[Variable] = Seq()
  var reducedCosts: Seq[Double] = Seq()

  //Populate
  //  def addConstraintIfNew(key: String): Unit = {
  //    if (!constraintIds.contains(key)) constraintIds = constraintIds :+ key
  //  }
  //  def addVarIfNew(key: String): Unit = {
  //    if (!varIds.contains(key)) varIds = varIds :+ key
  //  }
  def addConstraint(constraintType: String, elementId: String, inEquality: String, rhsValue: Double): String = {
    val constraintId = s"$constraintType.$elementId"
    if (!constraints.exists(c => c.constraintId == constraintId)) {
      constraints = constraints :+ Constraint(constraintId, constraintType, elementId, inEquality, rhsValue)
    }
    constraintId
  }

  def addVar(elementId: String, varType: String): String = {
    val varId = s"$elementId.$varType"
    if (!variables.exists(mathVar => mathVar.varId == varId)) {
      variables = variables :+ Variable(varId, varType, elementId)
    }
    varId
  }

  def setVarFactor(
                    varId: String,
                    constraintId: String,
                    value: Double
                  ): Unit = {
    val varFactor = VarFactor(varId, constraintId, value)
    if (!varFactors.contains(varFactor)) varFactors = varFactors :+ varFactor
    //    varFactors = varFactors.filter(v => !(v.varId == varId && v.constraintId == constraintId))
    //    varFactors = varFactors :+ VarFactor(varId,constraintId,value)
  }

  //Report
  def constraintsString: String = {
    constraintIds.mkString("\n")
  }
  def varsString: String = {
    varIds.mkString("\n")
  }
  def varFactorsString: String = {
    varFactors.map(_.toString).mkString("\n")
  }

  def varFactorsForConstraint(c: Constraint): Seq[Double] = {
    //If there is a varFactor for this constraint+var then add it otherwise add zero
    variables.map(v =>
      varFactors.find(vF => (vF.varId, vF.constraintId) == (v.varId, c.constraintId))
      match {
        case Some(optVF) => optVF.value
        case None => 0.0
      }
    )
  }

  //Solve
  def solveModel: String = {
    //Reduced costs for the objective
    //and a row of varFactors for each constraint
    //    for (c <- constraints.filter(_.constraintType != "objective")) {
    for (c <- constraints) {
      if (c.constraintType == "objective") { //expecting only one
        reducedCosts = varFactorsForConstraint(c)
      }
      //If there is a varFactor for this constraint+var then add it otherwise add zero
      //      val varFactorRow = variables.map(v =>
      //        varFactors.find(vF => (vF.varId, vF.constraintId) == (v.varId, c.constraintId))
      //        match {
      //          case Some(optVF) => optVF.value
      //          case None => 0.0
      //        }
      //      )
      else {
        varFactorRows :+= varFactorsForConstraint(c)
        //EQ has corresponding GTE
        if (c.inequality == "eq") varFactorRows :+= varFactorsForConstraint(c).map(vF => if (vF != 0) -vF else 0.0 )
      }
    }

    //Convert EQ constraints into LTE and GTE
    /*
    val convertedConstraints = constraints.map(c =>
      if (c.constraintType == "EQ") {

      }
      else {
        c
      }
    )*/

    s"${varFactorRows.map(_.toString).mkString("\n")} \nobjective\n${reducedCosts.toString()}"
  }

}
