package kamin.apl

import kamin.{Arithmetic, BooleanDefinition, IntegerValue, Reader, Relational, Value, cannotDivideWithZero}

import scala.util.{Failure, Success, Try}

case class VectorValue private(value: Vector[Int]) extends Value:
  override def isTrue: Boolean = this.value.exists(_ != 0)

  override def toString: String = value.mkString(" ") + "\n"

object VectorValue:
  def createVector(values: Seq[Int]): VectorValue =
    if (values.isEmpty)
      emptyVector
    else
      VectorValue(values.toVector)

  def emptyVector: VectorValue = VectorValue(Vector.empty)

trait VectorValueReader extends Reader:
  override def read(input: String): Either[String, Value] =
    if input.startsWith("'") && input.drop(1).trim.startsWith("(") && input.endsWith(")") then
      val content = input.drop(1).trim.drop(1).dropRight(1) // Remove `'` and parentheses
      Try(content.split("\\s+").map(_.toInt).toVector) match
        case Success(value) => Right(VectorValue.createVector(value))
        case Failure(_) => super.read(input)
    else
      super.read(input)


given Arithmetic[IntegerValue, VectorValue] with
  override def addition(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(v => operand1.value + v)))

  override def subtraction(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(v => operand1.value - v)))

  override def multiplication(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(v => operand1.value * v)))

  override def division(operand1: IntegerValue, operand2: VectorValue): Either[String, Value] =
    if (operand2.value.exists(_ == 0))
      cannotDivideWithZero
    else
      Right(VectorValue.createVector(operand2.value.map(v => operand1.value / v)))

given Arithmetic[VectorValue, IntegerValue] with
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

given Arithmetic[VectorValue, VectorValue] with
  override def addition(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map((v1, v2) => v1+v2)))
    else if operand1.value.length == 1 then
      given_Arithmetic_IntegerValue_VectorValue.addition(IntegerValue(operand1.value.head), operand2)
    else if operand2.value.length == 1 then
      given_Arithmetic_VectorValue_IntegerValue.addition(operand1, IntegerValue(operand2.value.head))
    else
      notOfSameShape

  override def subtraction(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map((v1, v2) => v1-v2)))
    else if operand1.value.length == 1 then
      given_Arithmetic_IntegerValue_VectorValue.multiplication(IntegerValue(operand1.value.head), operand2)
    else if operand2.value.length == 1 then
      given_Arithmetic_VectorValue_IntegerValue.multiplication(operand1, IntegerValue(operand2.value.head))
    else
      notOfSameShape

  override def multiplication(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map((v1, v2) => v1 * v2)))
    else if operand1.value.length == 1 then
      given_Arithmetic_IntegerValue_VectorValue.multiplication(IntegerValue(operand1.value.head), operand2)
    else if operand2.value.length == 1 then
      given_Arithmetic_VectorValue_IntegerValue.multiplication(operand1, IntegerValue(operand2.value.head))
    else
      notOfSameShape

  override def division(operand1: VectorValue, operand2: VectorValue): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      if (containsZero(operand2))
        cannotDivideWithZero
      else
        Right(VectorValue.createVector(operand1.value.zip(operand2.value).map((v1, v2) => v1 / v2)))
    else if operand1.value.length == 1 then
      given_Arithmetic_IntegerValue_VectorValue.division(IntegerValue(operand1.value.head), operand2)
    else if operand2.value.length == 1 then
      given_Arithmetic_VectorValue_IntegerValue.division(operand1, IntegerValue(operand2.value.head))
    else
      notOfSameShape

given Relational[VectorValue, IntegerValue] with
  override def equal(operand1: VectorValue, operand2: IntegerValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(VectorValue.createVector(operand1.value.map(e => toInteger(e == operand2.value))))

  override def greaterThan(operand1: VectorValue, operand2: IntegerValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(VectorValue.createVector(operand1.value.map(e => toInteger(e > operand2.value))))

  override def lessThan(operand1: VectorValue, operand2: IntegerValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(VectorValue.createVector(operand1.value.map(e => toInteger(e < operand2.value))))

given Relational[IntegerValue, VectorValue] with
  override def equal(operand1: IntegerValue, operand2: VectorValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(e => toInteger(e == operand1.value))))

  override def greaterThan(operand1: IntegerValue, operand2: VectorValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(e => toInteger(operand1.value > e))))

  override def lessThan(operand1: IntegerValue, operand2: VectorValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(VectorValue.createVector(operand2.value.map(e => toInteger(operand1.value < e))))

given Relational[VectorValue, VectorValue] with
  override def equal(operand1: VectorValue, operand2: VectorValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map { case (v1, v2) => toInteger(v1 == v2) }))
    else if operand1.value.length == 1 then
      given_Relational_IntegerValue_VectorValue.equal(IntegerValue(operand1.value.head), operand2)
    else if operand2.value.length == 1 then
      given_Relational_VectorValue_IntegerValue.equal(operand1, IntegerValue(operand2.value.head))
    else
      notOfSameShape

  override def greaterThan(operand1: VectorValue, operand2: VectorValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map { case (v1, v2) => toInteger(v1 > v2) }))
    else if operand1.value.length == 1 then
      given_Relational_IntegerValue_VectorValue.greaterThan(IntegerValue(operand1.value.head), operand2)
    else if operand2.value.length == 1 then
      given_Relational_VectorValue_IntegerValue.greaterThan(operand1, IntegerValue(operand2.value.head))
    else
      notOfSameShape

  override def lessThan(operand1: VectorValue, operand2: VectorValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    if shape(operand1) == shape(operand2) then
      Right(VectorValue.createVector(operand1.value.zip(operand2.value).map { case (v1, v2) => toInteger(v1 < v2) }))
    else if operand1.value.length == 1 then
      given_Relational_IntegerValue_VectorValue.lessThan(IntegerValue(operand1.value.head), operand2)
    else if operand2.value.length == 1 then
      given_Relational_VectorValue_IntegerValue.lessThan(operand1, IntegerValue(operand2.value.head))
    else
      notOfSameShape
