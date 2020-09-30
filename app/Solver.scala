package solver

import play.api.libs.json._

object MathModel {

  case class Element(elementId: String, bus: String)

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
      rhsValue: Double,
      multProperty: String
  )

  case class ConstraintComp(
      constraintId: String,
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

}
