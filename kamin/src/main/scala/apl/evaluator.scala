package kamin.apl

import kamin.{Arithmetic, BooleanDefinition, Environment, ExpressionEvaluator, FunctionDefinitionTable, IntegerValue, Reader, Relational, Value, cannotDivideWithZero, evaluateParameters}
import kamin.apl.VectorValue.{createVector, emptyVector}

private def notOfSameShape = Left("The two operands are not of same shape")

private def containsZero(v: VectorValue): Boolean =
  v.value.contains(0)

private def isLogicalVector(vector: VectorValue) =
  vector.value.forall(e => e == IntegerValue.intTrue || e == IntegerValue.intFalse)

private def isShapeVector(vector: VectorValue) =
  vector.value.length <= 2

private def toInteger(boolean: Boolean): Int =
  if boolean then IntegerValue.intTrue else IntegerValue.intFalse

def shape(value: Value): Either[String, VectorValue] =
  value match
    case IntegerValue(i) => Right(VectorValue.emptyVector)
    case VectorValue(v) => Right(VectorValue.createVector(Seq(v.length)))
    case MatrixValue(m) if m == Vector.empty => Right(VectorValue.createVector(Seq(0)))
    case MatrixValue(m) => Right(VectorValue.createVector(Seq(m.length, m.head.length)))
    case v => Left(s"Unable to determine shape of an unknown type: ${v.getClass.getName}")


def doCompressVector(controlVector: Vector[Int], value: Vector[Int]) : Vector[Int] =
  controlVector.zip(value).collect { case (IntegerValue.intTrue, value) => value }

def doCompressMatrix(controlVector: Vector[Int], value: Vector[Vector[Int]]): Vector[Vector[Int]]=
  value.map(v => doCompressVector(controlVector, v))

def compress(controlVector: VectorValue, value: VectorValue): Either[String, Value] =
  if shape(controlVector) == shape(value) then
    if isLogicalVector(controlVector) then
      Right(VectorValue.createVector(doCompressVector(controlVector.value, value.value)))
    else Left("control vector consists of only 0 and 1")
  else notOfSameShape

def compress(controlVector: VectorValue, value: MatrixValue): Either[String, Value] =
  shape(value) match
    case Left(error) => Left(error)
    case Right(matrixShape) =>
      val matrixColumnShape = matrixShape.value(1)

      if controlVector.value.length == matrixColumnShape then
        if isLogicalVector(controlVector) then
          Right(MatrixValue.createMatrix(doCompressMatrix(controlVector.value, value.value)))
        else
          Left("Control vector must consist of only 0 and 1")
      else
        notOfSameShape

def raveling(value: Value): Either[String, VectorValue] =
  value match
    case IntegerValue(v) => Right(VectorValue.createVector(Seq(v)))
    case VectorValue(v) => Right(VectorValue.createVector(v))
    case MatrixValue(v) => Right(VectorValue.createVector(v.flatten))
    case _ => Left("Invalid parameters")


private def doRestructureInteger(value: Int, shape: Vector[Int]): Value =
  shape.length match
    case 0 => IntegerValue(value)
    case 1 => VectorValue.createVector(Vector.fill(shape(0))(value))
    case 2 => MatrixValue.createMatrix(Vector.fill(shape(0))(Vector.fill(shape(1))(value)))

private def createMatrixFromVector(value: Vector[Int], shape: Vector[Int]): Vector[Vector[Int]] = {
  val repeatedValues = Vector.fill((shape.head * shape(1)) / value.length)(value).flatten ++ value.take((shape.head * shape(1)) % value.length)
  repeatedValues.grouped(shape(1)).toVector
}

private def createVectorFromVector(value: Vector[Int], shape: Vector[Int]) = {
  Vector.fill(shape.head / value.length)(value).flatten ++ value.take(shape.head % value.length)
}

private def doRestructureVector(value: Vector[Int], shape: Vector[Int]): Value =
  shape.length match
    case 0 => IntegerValue(value.head)
    case 1 => VectorValue.createVector (createVectorFromVector(value, shape))
    case 2 => MatrixValue.createMatrix( createMatrixFromVector(value, shape))

private def doRestructureMatrix(value: Vector[Vector[Int]], shape: Vector[Int]): Value =
  val flatten = value.flatten
  shape.length match
    case 0 => IntegerValue(flatten.head)
    case 1 => VectorValue.createVector(createVectorFromVector(flatten, shape))
    case 2 => MatrixValue.createMatrix(createMatrixFromVector(flatten, shape))

private def compensateForZeroVector(shape: Vector[Int]) : Vector[Int] =
  if (shape.length == 1 && shape.head == 0)
    Vector()
  else
    shape

def restructuring(shape: VectorValue, value: Value): Either[String, Value]=
  if isShapeVector(shape) then
    value match
      case IntegerValue(v) => Right(doRestructureInteger(v, compensateForZeroVector(shape.value)))
      case VectorValue(v) => Right(doRestructureVector(v, compensateForZeroVector(shape.value)))
      case MatrixValue(v) => Right(doRestructureMatrix(v, compensateForZeroVector(shape.value)))
      case _ => Left("Invalid parameters")
  else
    Left("A shape vector is a Vector of length 0, 1 or 2")

def catenation(value1: Value, value2: Value): Either[String, VectorValue] =
  raveling(value1).flatMap { raveled1 =>
    raveling(value2).flatMap { raveled2 =>
      Right(VectorValue.createVector(raveled1.value ++ raveled2.value))
    }
  }

def indexGeneration(index: Value): Either[String, VectorValue] =
  index match
    case v: IntegerValue => Right(VectorValue.createVector(Seq.range(1, v.value + 1)))
    case v: VectorValue if v.value.nonEmpty => Right(VectorValue.createVector(Seq.range(1, v.value.head + 1)))
    case v: MatrixValue if v.value.nonEmpty => Right(VectorValue.createVector(Seq.range(1, v.value.head.head + 1)))
    case _ => Left("Invalid parameters")


def transposition(value: Value): Either[String, Value] =
  value match
    case v: IntegerValue => Right(v)
    case v: VectorValue => Right(v)
    case v: MatrixValue => Right(MatrixValue.createMatrix(v.value.transpose))
    case _ => Left("Invalid parameters")

def subscripting(value: Value, index: VectorValue): Either[String, Value] =
  val idx = index.value.map(_ - 1)
  value match
    case v: VectorValue if idx.exists(i => i < 0 || i >= v.value.length) => Left("Invalid subscripts")
    case v: VectorValue => Right(VectorValue.createVector(idx.map(v.value)))
    case v: MatrixValue if idx.exists(i => i < 0 || i >= v.value.length) => Left("Invalid subscripts")
    case v: MatrixValue => Right(MatrixValue.createMatrix(idx.map(v.value)))
    case _ => Left("Invalid parameters")

def evaluateBinaryOperation[T <: Value](
                                         operand1: Value,
                                         operand2: Value,
                                         op: (Int, Int) => Int
                                       ): Either[String, Value] =
  (operand1, operand2) match
  case (IntegerValue(v1), IntegerValue(v2)) =>
    Right(IntegerValue(op(v1, v2)))
  case (IntegerValue(v1), VectorValue(v2)) =>
    Right(VectorValue.createVector(v2.map(op(v1, _))))
  case (VectorValue(v1), IntegerValue(v2)) =>
    Right(VectorValue.createVector(v1.map(op(_, v2))))
  case (vector1: VectorValue, vector2: VectorValue) if shape(vector1) == shape(vector2)  =>
    Right(VectorValue.createVector(
      vector1.value.zip(vector2.value).map { case (v1, v2) => op(v1, v2)}
    ))
  case (IntegerValue(v1), MatrixValue(m2)) =>
    Right(MatrixValue.createMatrix(m2.map(_.map(op(v1, _)))))
  case (MatrixValue(m1), IntegerValue(v2)) =>
    Right(MatrixValue.createMatrix(m1.map(_.map(op(_, v2)))))
  case (m1: MatrixValue, m2:MatrixValue) if shape(m1) == shape(m2) =>
    Right(MatrixValue.createMatrix(
      m1.value.zip(m2.value).map { case (row1, row2) =>
        row1.zip(row2).map { case (v1, v2) =>
          op(v1, v2)
        }
      }
    ))
  case _ => notOfSameShape

given ExpressionEvaluator[VectorValueExpressionNode] with
  extension (t: VectorValueExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                           (using functionDefinitionTable: FunctionDefinitionTable)
                                                                           (using reader: Reader)
                                                                           (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(t.vectorValue)


given ExpressionEvaluator[MaximumExpressionNode] with
  extension (t: MaximumExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                             (using functionDefinitionTable: FunctionDefinitionTable)
                                                                             (using reader: Reader)
                                                                             (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand1, operand2) =>
        evaluateBinaryOperation(operand1, operand2, Math.max)
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[AndExpressionNode] with
  extension (t: AndExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                        (using functionDefinitionTable: FunctionDefinitionTable)
                                                                        (using reader: Reader)
                                                                        (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
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
                                                                  (using reader: Reader)
                                                                 (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand1, operand2) =>
        evaluateBinaryOperation(
          operand1,
          operand2,
          (v1, v2) => toInteger(v1 != IntegerValue.intFalse || v2 != IntegerValue.intFalse),
        )
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[AdditionReductionExpressionNode] with
  extension (t: AdditionReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                 (using functionDefinitionTable: FunctionDefinitionTable)
                                                                 (using reader: Reader)
                                                                 (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: VectorValue) if operand == VectorValue.emptyVector => Right(VectorValue.emptyVector)
      case List(operand: VectorValue) => Right(IntegerValue(operand.value.sum))
      case List(operand: MatrixValue) => Right(MatrixValue.createMatrix(operand.value.map(row => Vector(row.sum))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[SubtractionReductionExpressionNode] with
  extension (t: SubtractionReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                (using reader: Reader)
                                                                                (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: VectorValue) if operand == VectorValue.emptyVector => Right(VectorValue.emptyVector)
      case List(operand: VectorValue) => Right(IntegerValue(operand.value.reduce(_ - _)))
      case List(operand: MatrixValue) => Right(MatrixValue.createMatrix(operand.value.map(row => Vector(row.reduce(_ - _)))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[MultiplicationReductionExpressionNode] with
  extension (t: MultiplicationReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                   (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                   (using reader: Reader)
                                                                                      (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: VectorValue) if operand == VectorValue.emptyVector => Right(VectorValue.emptyVector)
      case List(operand: VectorValue) => Right(IntegerValue(operand.value.product))
      case List(operand: MatrixValue) => Right(MatrixValue.createMatrix(operand.value.map(row => Vector(row.product))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[DivisionReductionExpressionNode] with
  extension (t: DivisionReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                      (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                      (using reader: Reader)
                                                                                      (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: VectorValue) if operand == VectorValue.emptyVector => Right(VectorValue.emptyVector)
      case List(operand: VectorValue) if operand.value.tail.contains(0) => cannotDivideWithZero
      case List(operand: VectorValue) => Right(IntegerValue(operand.value.reduce(_/_)))
      case List(operand: MatrixValue) => Right(MatrixValue.createMatrix(operand.value.map(row => Vector(row.reduce(_ / _)))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[MaximumReductionExpressionNode] with
  extension (t: MaximumReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                (using reader: Reader)
                                                                               (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand: IntegerValue) => Right(operand)
      case List(operand: VectorValue) if operand == VectorValue.emptyVector => Right(VectorValue.emptyVector)
      case List(operand: VectorValue) => Right(IntegerValue(operand.value.max))
      case List(operand: MatrixValue) => Right(MatrixValue.createMatrix(operand.value.map(row => Vector(row.max))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[AndReductionExpressionNode] with
  extension (t: AndReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                               (using functionDefinitionTable: FunctionDefinitionTable)
                                                                               (using reader: Reader)
                                                                                (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand: IntegerValue) => Right(if operand.value == IntegerValue.intFalse then IntegerValue.False else IntegerValue.True)
      case List(operand: VectorValue) if operand == VectorValue.emptyVector => Right(VectorValue.emptyVector)
      case List(operand: VectorValue) => Right(if operand.value.contains(IntegerValue.intFalse) then IntegerValue.False else IntegerValue.True)
      case List(operand: MatrixValue) =>
        Right(MatrixValue.createMatrix(operand.value.map(row => Vector(toInteger(!row.contains(IntegerValue.intFalse))))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[OrReductionExpressionNode] with
  extension (t: OrReductionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                           (using functionDefinitionTable: FunctionDefinitionTable)
                                                                           (using reader: Reader)
                                                                            (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand: IntegerValue) => Right(if operand.value == IntegerValue.intFalse then IntegerValue.False else IntegerValue.True)
      case List(operand: VectorValue) if operand == VectorValue.emptyVector => Right(VectorValue.emptyVector)
      case List(operand: VectorValue) => Right(if operand.value.exists(_ != IntegerValue.intFalse) then IntegerValue.True else IntegerValue.False)
      case List(operand: MatrixValue) =>
        Right(MatrixValue.createMatrix(operand.value.map(row => Vector(toInteger(row.exists(_ != IntegerValue.intFalse))))))
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[CompressionExpressionNode] with
  extension (t: CompressionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                                (using functionDefinitionTable: FunctionDefinitionTable)
                                                                                (using reader: Reader)
                                                                                (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand1: VectorValue, operand2: VectorValue) => compress(operand1, operand2)
      case List(operand1: VectorValue, operand2: MatrixValue) => compress(operand1, operand2)
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[ShapeExpressionNode] with
  extension (t: ShapeExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader)
                                                                          (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(value) => shape(value)
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[RavelingExpressionNode] with
  extension (t: RavelingExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                    (using functionDefinitionTable: FunctionDefinitionTable)
                                                                    (using reader: Reader)
                                                                    (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand) =>
        raveling(operand)
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[RestructuringExpressionNode] with
  extension (t: RestructuringExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader)
                                                                          (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(shape: IntegerValue, value: Value) => restructuring(VectorValue.createVector(Seq(shape.value)), value)
      case List(shape: VectorValue, value) => restructuring(shape, value)
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[CatenationExpressionNode] with
  extension (t: CatenationExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                            (using functionDefinitionTable: FunctionDefinitionTable)
                                                                            (using reader: Reader)
                                                                            (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand1, operand2) => catenation(operand1, operand2)
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[IndexGenerationExpressionNode] with
  extension (t: IndexGenerationExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                                                         (using reader: Reader)
                                                                         (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand) => indexGeneration(operand)
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[TranspositionExpressionNode] with
  extension (t: TranspositionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                              (using functionDefinitionTable: FunctionDefinitionTable)
                                                                              (using reader: Reader)
                                                                              (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand) => transposition(operand)
      case _ => Left("Invalid parameters")
    }

given ExpressionEvaluator[SubscriptingExpressionNode] with
  extension (t: SubscriptingExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                                                         (using reader: Reader)
                                                                         (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      case List(operand1: VectorValue, operand2: IntegerValue) =>
        subscripting(operand1, VectorValue.createVector(Seq(operand2.value)))
      case List(operand1: MatrixValue, operand2: IntegerValue) =>
        subscripting(operand1, VectorValue.createVector(Seq(operand2.value)))
      case List(operand1: VectorValue, operand2: VectorValue) =>
        subscripting(operand1, operand2)
      case List(operand1: MatrixValue, operand2: VectorValue) =>
        subscripting(operand1, operand2)
      case _ => Left("Invalid parameters")
    }
