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

  //Solver Defs
  case class VarFactor(
                        varId: String,
                        constraintId: String,
                        value: Double
                      )
  case class Constraint(
                       constraintType: String,
                       elementId: String
                       )
  case class Variable(
                         varType: String,
                         elementId: String
                       )

  //Data
  var constraintIds: Seq[String] = Seq()
  var varIds: Seq[String] = Seq()

  var rowVarFactors: Seq[Double] = Seq()
  var varFactors: Seq[VarFactor] = Seq()
  var constraints: Seq[Constraint] = Seq()
  var variables: Seq[Variable] = Seq()

  //Populate
  def addConstraintIfNew(key: String): Unit = {
    if (!constraintIds.contains(key)) {
      constraintIds = constraintIds :+ key
    }
  }
  def addVarIfNew(key: String): Unit = {
    if (!varIds.contains(key)) {
      varIds = varIds :+ key
    }
  }
  def setVarFactor(
      varId: String,
      constraintId: String,
      value: Double
  ): Unit = {
    varFactors = varFactors.filter(v => !(v.varId == varId && v.constraintId == constraintId))
    varFactors = varFactors :+ VarFactor(varId,constraintId,value)
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

  //Solve
  def solve: Unit = {
    //Create the matrix of constraint rows and var columns
//    for (constraintId <- constraintIds.filter){
//      
//    }
  }

}
