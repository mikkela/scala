package kamin.apl

import kamin.{Arithmetic, Environment, ExpressionEvaluator, FunctionDefinitionTable, IntegerValue, Reader, Relational, Value, evaluateParameters}
import kamin.apl.ValueExtensions.shape
import kamin.apl.VectorValue.createVector

private def cannotDivideWithZero = Left("Cannot divide with zero")
private def notOfSameShape = Left("The two operands are not of same shape")

private def containsZero(v: VectorValue): Boolean =
  v.value.exists(_ == 0)

private def hasSameShape(v1: Vector[Vector[Int]], v2: Vector[Vector[Int]]): Boolean =
  v1.length == v2.length &&
    v1.head.length == v2.head.length

private def toInteger(boolean: Boolean): Int =
  if boolean then IntegerValue.intTrue else IntegerValue.intFalse

def evaluateBinaryOperation[T <: Value](
                                         operand1: Value,
                                         operand2: Value,
                                         op: (Int, Int) => Int
                                       ): Either[String, Value] = (operand1, operand2) match
  case (IntegerValue(v1), IntegerValue(v2)) =>
    Right(IntegerValue(op(v1, v2)))
  case (IntegerValue(v1), MatrixValue(m2)) =>
    Right(MatrixValue(m2.map(_.map(op(v1, _)))))
  case (MatrixValue(m1), IntegerValue(v2)) =>
    Right(MatrixValue(m1.map(_.map(op(_, v2)))))
  case (MatrixValue(m1), MatrixValue(m2)) if hasSameShape(m1, m2) =>
    Right(MatrixValue(
      m1.zip(m2).map { case (row1, row2) =>
        row1.zip(row2).map { case (v1, v2) =>
          op(v1, v2)
        }
      }
    ))
  case _ => notOfSameShape

given ExpressionEvaluator[VectorValueExpressionNode] with
  extension (t: VectorValueExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                           (using functionDefinitionTable: FunctionDefinitionTable)
                                                                           (using reader: Reader): Either[String, Value] =
    Right(t.vectorValue)


given ExpressionEvaluator[MaximumExpressionNode] with
  extension (t: MaximumExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                             (using functionDefinitionTable: FunctionDefinitionTable)
                                                                             (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader).flatMap {
      case List(operand1, operand2) =>
        evaluateBinaryOperation(operand1, operand2, Math.max)
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[AndExpressionNode] with
  extension (t: AndExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                        (using functionDefinitionTable: FunctionDefinitionTable)
                                                                        (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader).flatMap {
      case List(operand1, operand2) =>
        evaluateBinaryOperation(
          operand1,
          operand2,
          (v1, v2) => toInteger(v1 != IntegerValue.intFalse && v2 != IntegerValue.intFalse),
      )
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[OrExpressionNode] with
  extension (t: OrExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                  (using functionDefinitionTable: FunctionDefinitionTable)
                                                                  (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader).flatMap {
      case List(operand1, operand2) =>
        evaluateBinaryOperation(
          operand1,
          operand2,
          (v1, v2) => toInteger(v1 != IntegerValue.intFalse || v2 != IntegerValue.intFalse),
        )
      case _ => Left("Invalid parameters")
    }

private def isNullMatrix(m: MatrixValue) = m.value.length == 0
private def nullMatrix = MatrixValue(Vector.empty)
private def isVector(m: MatrixValue) = m.value.length == 1

given ExpressionEvaluator[AdditionReductionExpressionNode] with
  extension (t: AdditionReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                 (using functionDefinitionTable: FunctionDefinitionTable)
                                                                 (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: MatrixValue) if isNullMatrix(operand) => Right(operand)
      case List(operand: MatrixValue) if isVector(operand) => Right(IntegerValue(operand.value.head.sum))
      case List(operand: MatrixValue) => Right(MatrixValue(operand.value.map(row => Vector(row.sum))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[SubtractionReductionExpressionNode] with
  extension (t: SubtractionReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: MatrixValue) if isNullMatrix(operand) => Right(operand)
      case List(operand: MatrixValue) if isVector(operand) => Right(IntegerValue(operand.value.head.reduce(_ - _)))
      case List(operand: MatrixValue) => Right(MatrixValue(operand.value.map(row => Vector(row.reduce(_ - _)))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[MultiplicationReductionExpressionNode] with
  extension (t: MultiplicationReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                   (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                   (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: MatrixValue) if isNullMatrix(operand) => Right(operand)
      case List(operand: MatrixValue) if isVector(operand) => Right(IntegerValue(operand.value.head.reduce(_ * _)))
      case List(operand: MatrixValue) => Right(MatrixValue(operand.value.map(row => Vector(row.reduce(_ * _)))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[DivisionReductionExpressionNode] with
  extension (t: DivisionReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                      (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                      (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: MatrixValue) if isNullMatrix(operand) => Right(operand)
      case List(operand: MatrixValue) if isVector(operand) => Right(IntegerValue(operand.value.head.reduce(_ / _)))
      case List(operand: MatrixValue) => Right(MatrixValue(operand.value.map(row => Vector(row.reduce(_ / _)))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[MaximumReductionExpressionNode] with
  extension (t: MaximumReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: MatrixValue) if isNullMatrix(operand) => Right(operand)
      case List(operand: MatrixValue) if isVector(operand) => Right(IntegerValue(operand.value.head.reduce(_ / _)))
      case List(operand: MatrixValue) => Right(MatrixValue(operand.value.map(row => Vector(row.max))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[AndReductionExpressionNode] with
  extension (t: AndReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                               (using functionDefinitionTable: FunctionDefinitionTable)
                                                                               (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: MatrixValue) if isNullMatrix(operand) => Right(operand)
      case List(operand: MatrixValue) if isVector(operand) =>
        Right(IntegerValue(toInteger(operand.value.head.forall(_ != IntegerValue.intFalse))))
      case List(operand: MatrixValue) =>
        Right(MatrixValue(operand.value.map(row => Vector(toInteger(row.forall(_ != IntegerValue.intFalse))))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[OrReductionExpressionNode] with
  extension (t: OrReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                           (using functionDefinitionTable: FunctionDefinitionTable)
                                                                           (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: MatrixValue) if isNullMatrix(operand) => Right(operand)
      case List(operand: MatrixValue) if isVector(operand) =>
        Right(IntegerValue(toInteger(operand.value.head.exists(_ != IntegerValue.intFalse))))
      case List(operand: MatrixValue) =>
        Right(MatrixValue(operand.value.map(row => Vector(toInteger(row.exists(_ != IntegerValue.intFalse))))))
      case _ => Left("Invalid parameters")
    }

private def isLogicalVector(matrix: MatrixValue) =
  isVector(matrix) && matrix.value.head.forall(e => e == IntegerValue.intTrue || e == IntegerValue.intFalse)

given ExpressionEvaluator[CompressionExpressionNode] with
  extension (t: CompressionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader).flatMap {
      case List(operand1: MatrixValue, operand2: MatrixValue)
        if isLogicalVector(operand1) &&
          isVector(operand2) &&
          operand1.value.head.length == operand2.value.head.length =>
            Right(MatrixValue.fromVector(operand1.value.head.zip(operand2.value.head).collect {
              case (IntegerValue.intTrue, value) => value
            }))
      case List(operand1: MatrixValue, operand2: MatrixValue)
        if isLogicalVector(operand1) &&
          operand2.value.length == operand1.value.length =>
            Right(MatrixValue(operand2.value.map(row => row.map {
              case value if value == IntegerValue.intTrue => value // Perform your logic here
            })))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[ShapeExpressionNode] with
  extension (t: ShapeExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) =>
        Right(MatrixValue(Vector.empty))
      case List(operand: MatrixValue) if isNullMatrix(operand) =>
        Right(operand)
      case List(operand: MatrixValue) if isVector(operand) =>
        Right(MatrixValue.fromVector(Vector(operand.value.head.length)))
      case List(operand: MatrixValue) =>
        Right(MatrixValue.fromVector(Vector(operand.value.length, operand.value.head.length)))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[RavelingExpressionNode] with
  extension (t: RavelingExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                    (using functionDefinitionTable: FunctionDefinitionTable)
                                                                    (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) =>
        Right(MatrixValue.fromVector(Vector(operand.value)))
      case List(operand: MatrixValue) if isNullMatrix(operand) || isVector(operand) =>
        Right(operand)
      case List(operand: MatrixValue) =>
        Right(MatrixValue.fromVector(operand.value.flatten))
      case _ => Left("Invalid parameters")
    }

private def isShapeVector(m: MatrixValue) =
  isVector(m) && m.value.head.length <= 2

given ExpressionEvaluator[RestructuringExpressionNode] with
  extension (t: RestructuringExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader).flatMap {
      case List(_, operand2: MatrixValue) if isNullMatrix(operand2) => Right(nullMatrix)
      case List(operand1: MatrixValue, operand2: IntegerValue) if isNullMatrix(operand1) =>
        Right(MatrixValue.fromVector(Vector(operand2.value)))
      case List(operand1: MatrixValue, operand2: MatrixValue) if isNullMatrix(operand1) =>
        Right(MatrixValue.fromVector(Vector(operand2.value.head.head)))
      case List(operand1: MatrixValue, operand2: IntegerValue) if isShapeVector(operand1) =>
        val shape = operand1.value.head
        if shape.length == 1 then
          Right(MatrixValue.fromVector(Vector.fill(shape(0))(operand2.value)))
        else
          Right(MatrixValue(Vector.fill(shape(0))(Vector.fill(shape(1))(operand2.value))))
      case List(operand1: MatrixValue, operand2: MatrixValue) if isShapeVector(operand1) =>
        val shape = operand1.value.head
        val candidates = Iterator.continually(operand2.value.flatten).flatten
        if shape.length == 1 then
          Right(MatrixValue.fromVector(candidates.take(shape(0)).toVector))
        else
          Right(MatrixValue(candidates.take(shape(0)*shape(1)).grouped(shape(1)).map(_.toVector).toVector))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[CatenationExpressionNode] with
  extension (t: CatenationExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                            (using functionDefinitionTable: FunctionDefinitionTable)
                                                                            (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader).flatMap {
      case List(operand1: IntegerValue, operand2: IntegerValue) =>
        Right(MatrixValue.fromVector(Vector(operand1.value, operand2.value)))
      case List(operand1: IntegerValue, operand2: MatrixValue) =>
        Right(MatrixValue.fromVector(operand1.value +: operand2.value.flatten))
      case List(operand1: MatrixValue, operand2: IntegerValue)  =>
        Right(MatrixValue.fromVector(operand1.value.flatten :+ operand2.value))
      case List(operand1: MatrixValue, operand2: MatrixValue) =>
        Right(MatrixValue.fromVector(operand1.value.flatten ++ operand2.value.flatten))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[IndexGenerationExpressionNode] with
  extension (t: IndexGenerationExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                                                         (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) if operand.value >= 1 =>
        Right(MatrixValue.fromVector(Vector.range(1, operand.value)))
      case List(operand: MatrixValue) if !isNullMatrix(operand) && operand.value.head(0) >= 1 =>
        Right(MatrixValue.fromVector(Vector.range(1, operand.value.head(0))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[TranspositionExpressionNode] with
  extension (t: TranspositionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                              (using functionDefinitionTable: FunctionDefinitionTable)
                                                                              (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: MatrixValue) if isNullMatrix(operand) || isVector(operand) => Right(operand)
      case List(operand: MatrixValue) => Right(MatrixValue(operand.value.transpose))
      case _ => Left("Invalid parameters")
    }

private def isValidIndexVectorForMatrix(indexVector: Vector[Int], matrix: Vector[Vector[Int]]) =
  indexVector.min > 0 && indexVector.max <= matrix.head.length && (matrix.length == 1 || indexVector.max <= matrix.length)

given ExpressionEvaluator[SubscriptingExpressionNode] with
  extension (t: SubscriptingExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                                                         (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader).flatMap {
      case List(operand1: MatrixValue, operand2: IntegerValue)
        if !isNullMatrix(operand1) && isValidIndexVectorForMatrix(Vector(operand2.value), operand1.value) =>
        Right(MatrixValue.fromVector(operand1.value.map(row => row(operand2.value - 1))))
      case List(operand1: MatrixValue, operand2: MatrixValue)
        if !isNullMatrix(operand1) && isVector(operand2) &&
          isValidIndexVectorForMatrix(operand2.value.head, operand1.value) =>
        val zeroBased = operand2.value.head.map(_ - 1)
        Right(MatrixValue(operand1.value.zipWithIndex.collect {
          case (value, index) if zeroBased.contains(index) => value
        }.map(row => zeroBased.map(row))))
      case _ => Left("Invalid parameters")
    }

given Arithmetic[IntegerValue, MatrixValue] with
  override def addition(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue(operand2.value.map( row => row.map(operand1.value + _))))

  override def subtraction(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue(operand2.value.map( row => row.map(operand1.value - _))))

  override def multiplication(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue(operand2.value.map( row => row.map(operand1.value * _))))

  override def division(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue(operand2.value.map( row => row.map(operand1.value / _))))

given Relational[IntegerValue, MatrixValue] with
  override def equal(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue(operand2.value.map( row => row.map(
      e => toInteger(e == operand1.value)))))

  override def greaterThan(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue(operand2.value.map( row => row.map(
      e => toInteger(operand1.value > e)))))

  override def lessThan(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue(operand2.value.map( row => row.map(
      e => toInteger(operand1.value < e)))))


object VectorIntegerArithmetic extends Arithmetic[VectorValue, IntegerValue]:
  override def addition(operand1: VectorValue, operand2: IntegerValue): Either[String, Value] =
    Right(VectorValue.createVector(operand1.value.map(v => v + operand2.value)))

  override def subtraction(operand1: VectorValue, operand2: IntegerValue): Either[String, Value] =
    Right(VectorValue.createVector(operand1.value.map(v => v - operand2.value)))

  override def multiplication(operand1: VectorValue, operand2: IntegerValue): Either[String, Value] =
    Right(VectorValue.createVector(operand1.value.map(v => v * operand2.value)))

  override def division(operand1: VectorValue, operand2: IntegerValue): Either[String, Value] =
    if (operand2 == IntegerValue.Zero)
      cannotDivideWithZero
    else
      Right(VectorValue.createVector(operand1.value.map(v => v / operand2.value)))

object VectorIntegerRelational extends Relational[VectorValue, IntegerValue]:
  override def equal(operand1: VectorValue, operand2: IntegerValue): Either[String, Value] =
    Right(VectorValue.createVector(operand1.value.map(e => toInteger(e == operand2.value))))

  override def greaterThan(operand1: VectorValue, operand2: IntegerValue): Either[String, Value] =
    Right(VectorValue.createVector(operand1.value.map(e => toInteger(operand2.value > e))))

  override def lessThan(operand1: VectorValue, operand2: IntegerValue): Either[String, Value] =
    Right(VectorValue.createVector(operand1.value.map(e => toInteger(operand2.value < e))))

object IntegerVectorArithmetic extends Arithmetic[IntegerValue, VectorValue]:
  override def addition(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(v => operand1.value + v)))

  override def subtraction(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(v => operand1.value - v)))

  override def multiplication(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(v => operand1.value * v)))

  override def division(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    if (containsZero(operand2))
      cannotDivideWithZero
    else
      Right(VectorValue.createVector(operand2.value.map(v => operand1.value / v)))

object IntegerVectorRelational extends Relational[IntegerValue, VectorValue]:
  override def equal(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(e => toInteger(e == operand1.value))))

  override def greaterThan(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(e => toInteger(operand1.value > e))))

  override def lessThan(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(e => toInteger(operand1.value < e))))


object VectorVectorArithmetic extends Arithmetic[VectorValue, VectorValue]:
  override def addition(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if operand1.shape == operand2.shape then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map((v1, v2) => v1+v2)))
    else
      notOfSameShape

  override def subtraction(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if operand1.shape == operand2.shape then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map((v1, v2) => v1 - v2)))
    else
      notOfSameShape

  override def multiplication(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if operand1.shape == operand2.shape then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map((v1, v2) => v1 * v2)))
    else
      notOfSameShape

  override def division(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if operand1.shape == operand2.shape then
      if (containsZero(operand2))
        cannotDivideWithZero
      else
        Right(VectorValue.createVector(operand1.value.zip(operand2.value).map((v1, v2) => v1 / v2)))
    else
      notOfSameShape

object VectorVectorRelational extends Relational[VectorValue, VectorValue]:
  override def equal(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if operand1.shape == operand2.shape then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map { case (v1, v2) => toInteger(v1 == v2) }))
    else
      notOfSameShape

  override def greaterThan(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if operand1.shape == operand2.shape then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map { case (v1, v2) => toInteger(v1 > v2) }))
    else
      notOfSameShape

  override def lessThan(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if operand1.shape == operand2.shape then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map { case (v1, v2) => toInteger(v1 < v2) }))
    else
      notOfSameShape
/*given Arithmetic[MatrixValue, IntegerValue] with
  override def addition(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue(operand1.value.map( row => row.map(_ + operand2.value))))

  override def subtraction(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue(operand1.value.map( row => row.map(_ - operand2.value))))

  override def multiplication(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue(operand1.value.map( row => row.map(_ * operand2.value))))

  override def division(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue(operand1.value.map( row => row.map(_ / operand2.value))))

given Relational[MatrixValue, IntegerValue] with
  override def equal(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue(operand1.value.map( row => row.map(
      e => toInteger(e == operand2.value)))))

  override def greaterThan(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue(operand1.value.map( row => row.map(
      e => toInteger(e > operand2.value)))))

  override def lessThan(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue(operand1.value.map( row => row.map(
      e => toInteger(e < operand2.value)))))

given Arithmetic[MatrixValue, MatrixValue] with
  override def addition(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if hasSameShape(operand1.value, operand2.value) then
      Right(
        MatrixValue(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => v1 + v2 }
          }
        )
      )
    else
      differentShapes

  override def subtraction(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if hasSameShape(operand1.value, operand2.value) then
      Right(
        MatrixValue(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => v1 - v2 }
          }
        )
      )
    else
      differentShapes

  override def multiplication(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if hasSameShape(operand1.value, operand2.value) then
      Right(
        MatrixValue(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => v1 * v2 }
          }
        )
      )
    else
      differentShapes

  override def division(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if hasSameShape(operand1.value, operand2.value) then
      Right(
        MatrixValue(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => v1 / v2 }
          }
        )
      )
    else
      differentShapes

given Relational[MatrixValue, MatrixValue] with
  override def equal(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if hasSameShape(operand1.value, operand2.value) then
      Right(
        MatrixValue(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => toInteger(v1 == v2) }
          }
        )
      )
    else
      differentShapes

  override def greaterThan(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if hasSameShape(operand1.value, operand2.value) then
      Right(
        MatrixValue(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => toInteger(v1 > v2) }
          }
        )
      )
    else
      differentShapes

  override def lessThan(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if hasSameShape(operand1.value, operand2.value) then
      Right(
        MatrixValue(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => toInteger(v1 < v2) }
          }
        )
      )
    else
      differentShapes
*/
