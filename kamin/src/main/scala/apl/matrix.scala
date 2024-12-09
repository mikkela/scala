package kamin.apl

import kamin.{Arithmetic, BooleanDefinition, IntegerValue, Reader, Relational, Value, cannotDivideWithZero}

import scala.util.{Failure, Success, Try}

case class MatrixValue private(value: Vector[Vector[Int]]) extends Value:
  override def isTrue: Boolean = value.exists(_.exists(_ != 0))
  override def toString: String = value.map(_.mkString(" ")).mkString("\n")

object MatrixValue:
  def createMatrix(values: Seq[Seq[Int]]): MatrixValue =
    if (values.isEmpty || values.head.isEmpty) then
      emptyMatrix
    else
      MatrixValue(values.map(_.toVector).toVector)

  def emptyMatrix: MatrixValue = MatrixValue(Vector.empty)

given Arithmetic[IntegerValue, MatrixValue] with
  override def addition(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand2.value.map(row => row.map(operand1.value + _))))

  override def subtraction(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand2.value.map(row => row.map(operand1.value - _))))

  override def multiplication(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand2.value.map(row => row.map(operand1.value * _))))

  override def division(operand1: IntegerValue, operand2: MatrixValue): Either[String, Value] =
    if (operand2.value.exists(_.exists(_ == 0)))
      cannotDivideWithZero
    else
      Right(MatrixValue.createMatrix(operand2.value.map(row => row.map(operand1.value / _))))

given Arithmetic[MatrixValue, IntegerValue] with
  override def addition(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand1.value.map(row => row.map(_ + operand2.value))))

  override def subtraction(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand1.value.map(row => row.map(_ - operand2.value))))

  override def multiplication(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand1.value.map(row => row.map(_ * operand2.value))))

  override def division(operand1: MatrixValue, operand2: IntegerValue): Either[String, Value] =
    if (operand2 == IntegerValue.Zero)
      cannotDivideWithZero
    else
      Right(MatrixValue.createMatrix(operand1.value.map(row => row.map(_ / operand2.value))))

given Arithmetic[MatrixValue, MatrixValue] with
  override def addition(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(
        MatrixValue.createMatrix(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => v1 + v2 }
          }
        )
      )
    else if operand1.value.length == 1 && operand1.value.head.length == 1 then
      given_Arithmetic_IntegerValue_MatrixValue.addition(IntegerValue(operand1.value.head.head), operand2)
    else if operand2.value.length == 1 then
      given_Arithmetic_MatrixValue_IntegerValue.addition(operand1, IntegerValue(operand2.value.head.head))
    else
      notOfSameShape

  override def subtraction(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(
        MatrixValue.createMatrix(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => v1 - v2 }
          }
        )
      )
    else if operand1.value.length == 1 && operand1.value.head.length == 1 then
      given_Arithmetic_IntegerValue_MatrixValue.subtraction(IntegerValue(operand1.value.head.head), operand2)
    else if operand2.value.length == 1 then
      given_Arithmetic_MatrixValue_IntegerValue.subtraction(operand1, IntegerValue(operand2.value.head.head))
    else
      notOfSameShape

  override def multiplication(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(
        MatrixValue.createMatrix(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => v1 * v2 }
          }
        )
      )
    else if operand1.value.length == 1 && operand1.value.head.length == 1 then
      given_Arithmetic_IntegerValue_MatrixValue.multiplication(IntegerValue(operand1.value.head.head), operand2)
    else if operand2.value.length == 1 then
      given_Arithmetic_MatrixValue_IntegerValue.multiplication(operand1, IntegerValue(operand2.value.head.head))
    else
      notOfSameShape

  override def division(operand1: MatrixValue, operand2: MatrixValue): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      if operand2.value.exists(_.exists(_ == 0)) then
        cannotDivideWithZero
      else
        Right(
          MatrixValue.createMatrix(
            operand1.value.zip(operand2.value).map { case (row1, row2) =>
              row1.zip(row2).map { case (v1, v2) => v1 / v2 }
            }
          )
        )
    else if operand1.value.length == 1 && operand1.value.head.length == 1 then
      given_Arithmetic_IntegerValue_MatrixValue.division(IntegerValue(operand1.value.head.head), operand2)
    else if operand2.value.length == 1 then
      given_Arithmetic_MatrixValue_IntegerValue.division(operand1, IntegerValue(operand2.value.head.head))
    else
      notOfSameShape

given Relational[IntegerValue, MatrixValue] with
  override def equal(operand1: IntegerValue, operand2: MatrixValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand2.value.map( row => row.map(
      e => toInteger(e == operand1.value)))))

  override def greaterThan(operand1: IntegerValue, operand2: MatrixValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand2.value.map( row => row.map(
      e => toInteger(operand1.value > e)))))

  override def lessThan(operand1: IntegerValue, operand2: MatrixValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand2.value.map( row => row.map(
      e => toInteger(operand1.value < e)))))

given Relational[MatrixValue, IntegerValue] with
  override def equal(operand1: MatrixValue, operand2: IntegerValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand1.value.map( row => row.map(
      e => toInteger(e == operand2.value)))))

  override def greaterThan(operand1: MatrixValue, operand2: IntegerValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand1.value.map( row => row.map(
      e => toInteger(e > operand2.value)))))

  override def lessThan(operand1: MatrixValue, operand2: IntegerValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(MatrixValue.createMatrix(operand1.value.map( row => row.map(
      e => toInteger(e < operand2.value)))))

given Relational[MatrixValue, MatrixValue] with
  override def equal(operand1: MatrixValue, operand2: MatrixValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(
        MatrixValue.createMatrix(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => toInteger(v1 == v2) }
          }
        )
      )
    else if operand1.value.length == 1 && operand1.value.head.length == 1 then
      given_Relational_IntegerValue_MatrixValue.equal(IntegerValue(operand1.value.head.head), operand2)
    else if operand2.value.length == 1 then
      given_Relational_MatrixValue_IntegerValue.equal(operand1, IntegerValue(operand2.value.head.head))
    else
      notOfSameShape

  override def greaterThan(operand1: MatrixValue, operand2: MatrixValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(
        MatrixValue.createMatrix(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => toInteger(v1 > v2) }
          }
        )
      )
    else if operand1.value.length == 1 && operand1.value.head.length == 1 then
      given_Relational_IntegerValue_MatrixValue.greaterThan(IntegerValue(operand1.value.head.head), operand2)
    else if operand2.value.length == 1 then
      given_Relational_MatrixValue_IntegerValue.greaterThan(operand1, IntegerValue(operand2.value.head.head))
    else
      notOfSameShape

  override def lessThan(operand1: MatrixValue, operand2: MatrixValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(
        MatrixValue.createMatrix(
          operand1.value.zip(operand2.value).map { case (row1, row2) =>
            row1.zip(row2).map { case (v1, v2) => toInteger(v1 < v2) }
          }
        )
      )
    else if operand1.value.length == 1 && operand1.value.head.length == 1 then
      given_Relational_IntegerValue_MatrixValue.lessThan(IntegerValue(operand1.value.head.head), operand2)
    else if operand2.value.length == 1 then
      given_Relational_MatrixValue_IntegerValue.lessThan(operand1, IntegerValue(operand2.value.head.head))
    else
      notOfSameShape
