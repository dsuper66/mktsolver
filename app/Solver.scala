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
        case JsArray(arr) =>
          val list = arr.map(metaValueToJsValue)
          JsSuccess(list)
        case JsNull => JsSuccess(null)
        //case x => JsFailure(x.toString())
        case JsObject(m) =>
          val m1 = m.map(f => (f._1, metaValueToJsValue(f._2))).toMap
          JsSuccess(m1)
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
    implicit val reads: Reads[ConstraintComp] = Json.reads[ConstraintComp]
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
  //Var factor inputs are used to create varFactor rows
  //which are then related to c and v by row and col
  var varFactorInputs: Seq[VarFactor] = Seq()

  var constraints: Seq[Constraint] = Seq()
  var objectiveFn: Constraint = Constraint("", "", "", "", 0.0)
  var objectiveRhs: Double = 0.0

  var variables: Seq[Variable] = Seq()
  var reducedCosts: Seq[Double] = Seq()
  var rhsValues: Seq[Double] = Seq()
  var basicColIndexForRow:  Seq[Int] = Seq()

  //Populate
  def resetMathModel(): Unit = {
    varFactorRows = Seq()
    varFactorInputs = Seq()
    constraints = Seq()
    variables = Seq()
    reducedCosts = Seq()
    rhsValues = Seq()
    objectiveFn = Constraint("", "", "", "", 0.0)
    objectiveRhs = 0.0
    basicColIndexForRow = Seq()

  }

  def addConstraint(constraintType: String, elementId: String, inequality: String, rhsValue: Double): String = {
    val constraintId = s"$constraintType.$elementId"
    val newC = Constraint(constraintId, constraintType, elementId, inequality, rhsValue)
    if (constraintType == "objective") {
      objectiveFn = newC
    }
    else if (!constraints.exists(c => c.constraintId == constraintId)) {
      constraints = constraints :+ newC
    }
    constraintId
  }

  def addVar(elementId: String, varType: String): String = {
    val varId = s"$elementId.$varType"
    if (!variables.exists(v => v.varId == varId)) {
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
    if (!varFactorInputs.contains(varFactor)) varFactorInputs = varFactorInputs :+ varFactor
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
    varFactorInputs.map(_.toString).mkString("\n")
  }

  def varFactorsForConstraint(c: Constraint): Seq[Double] = {
    //If there is a varFactor for this constraint+var then add it otherwise add zero
    variables.map(v =>
      varFactorInputs.find(vF => (vF.varId, vF.constraintId) == (v.varId, c.constraintId))
      match {
        case Some(optVF) => optVF.value
        case None => 0.0
      }
    )
  }

  //Solve
  def solveModel: String = {
    //Reduced costs for the objective
    reducedCosts = varFactorsForConstraint(objectiveFn)

    //VarFactors for each constraint
    for (c <- constraints) {
      varFactorRows :+= varFactorsForConstraint(c)
      //EQ has corresponding GTE
      if (c.inequality == "eq") varFactorRows :+= varFactorsForConstraint(c).map(vF => if (vF != 0) -vF else 0.0)
    }

    //Convert EQ constraints into LTE and GTE
    var constraintsWithEq: Seq[Constraint] = Seq()
    for (c <- constraints) {
      constraintsWithEq = constraintsWithEq ++ {
        c.inequality
        match {
          case "eq" => Seq(Constraint(s"${c.constraintId}.LTE", c.constraintType, c.elementId, c.inequality, c.rhsValue),
            Constraint(s"${c.constraintId}.GTE", c.constraintType, c.elementId, c.inequality, c.rhsValue))
          case _ => Seq(c)
        }
      }
    }
    constraints = constraintsWithEq

    //RHS
    rhsValues = constraints.map(c => c.rhsValue)

    //Add slack vars...
    //...varFactors rows
    varFactorRows = varFactorRows.map(
      row => row ++ Seq.tabulate(constraints.length)(col => if (col == varFactorRows.indexOf(row)) 1.0 else 0.0))
    //...reducedCosts row
    reducedCosts = reducedCosts ++ Seq.fill(constraints.length)(0.0)

    //Record index of basic cols
    basicColIndexForRow = Seq.tabulate(constraints.length)(col => variables.length + col)

    var msg = "******************************************************"

//    enteringColNum = reducedCosts.zipWithIndex.filter{
//      case(colValue,colIndex) => colValue < 0}.find{case(colValue,colIndex) => colValue == reducedCosts.min}.map{
//      case (colValue,colIndex)=> colIndex}.getOrElse(-1)

    var enteringColNum = {
      val ltZeroSeq = reducedCosts.zipWithIndex.filter{case(colValue,colIndex) => colValue < 0}
      if (ltZeroSeq.length > 0) ltZeroSeq.minBy{case(colValue,colIndex) => colValue}._2
      else -1
    }

    //Add reduced costs and rhs to the varFactors
    var fullMatrix = (varFactorRows :+ reducedCosts).zipWithIndex.map { case (rowValues, rowIndex) =>
      rowValues :+ (rhsValues :+ objectiveRhs) (rowIndex)
    }

    //====Iterate====
    var iterationCount = 0
    while (enteringColNum >= 0 && iterationCount < 7) {

      val varFactorEnteringCol = varFactorRows.map(row => row(enteringColNum))
      //    val enteringRow = varFactorCol.zipWithIndex.filter(
      //      vF => vF._1 > 0).minBy(vF => rhsValues(vF._2)/vF._1)._2

      //Entering row is minimum ratio or rhs/factor where factor is > 0
      val enteringRowNum = varFactorEnteringCol.zipWithIndex.filter {
        case (rowValue,_) => rowValue > 0
      }.minBy { case (rowValue, index) => rhsValues(index) / rowValue }._2

      //    var stepVarFactorRows = varFactorRows.filter(row => varFactorRows.indexOf(row) != enteringRowNum)

      //Record the basic var
      basicColIndexForRow = basicColIndexForRow.updated(enteringRowNum,enteringColNum)

      val enteringRow = fullMatrix(enteringRowNum)
      //val pivotValue = fullMatrix(enteringRowNum)(enteringColNum)

      fullMatrix = fullMatrix.zipWithIndex.map { case (thisRow, rowIndex) =>
        if (rowIndex == enteringRowNum) thisRow
        else {
          thisRow(enteringColNum) match {
            case 0 => thisRow //thisRow is unchanged
            case _ => thisRow.zipWithIndex.map {
              case (colValue, colIndex) =>
                colValue - enteringRow(colIndex)*(thisRow(enteringColNum)/enteringRow(enteringColNum))
            }
          }
        }
      }

      //Var factors are full matrix without last row and last col
      varFactorRows = fullMatrix.dropRight(1).map(row => row.dropRight(1))

      //Reduced costs is last row, without rhs
      reducedCosts = fullMatrix(fullMatrix.length - 1).dropRight(1)

      //Reporting
      rhsValues = fullMatrix.dropRight(1).map(row => row(row.length - 1))

      println(s"\n*****$reducedCosts")
      val thisMsg = s"\n\n>>>iteration: $iterationCount\nenteringVarCol: $enteringColNum" +
        s"\nbasic cols: $basicColIndexForRow\nrhs: $rhsValues\nvarFactorCol: $varFactorEnteringCol" +
        s"\nentering row: $enteringRowNum\nfull matrix:\n${fullMatrix.map(_.toString).mkString("\n")}"
      println(thisMsg)
      msg += thisMsg


      var resultString = "\n####\n"
      for ((basicCol,rowIndex) <- basicColIndexForRow.zipWithIndex.filter(_._1 < variables.length)) {
        resultString += s"\nbc:$basicCol ri:$rowIndex ${variables(basicCol).varId} = ${rhsValues(rowIndex)}"
      }
      resultString += "\n####"
      println(resultString)
      msg += resultString



      enteringColNum = {
        val ltZeroSeq = reducedCosts.zipWithIndex.filter{case(colValue,colIndex) => colValue < 0}
        if (ltZeroSeq.length > 0) ltZeroSeq.minBy{case(colValue,colIndex) => colValue}._2
        else -1
      }

      iterationCount += 1
    }

    //            case 0 => 0
    //          }
    //          rhsValues(varFactorRows.indexOf(row))/row(enteringVarCol)
    //        }).filter(ratio => ratio >= 0.0).min
    //    }

    s"${varFactorRows.map(_.toString).mkString("\n")} \nreduced costs\n${reducedCosts.toString()} " +
      s"\nconstraints\n${constraints.map(_.toString).mkString("\n")}\n$msg"
  }

}
