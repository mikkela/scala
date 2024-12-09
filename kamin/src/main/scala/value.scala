package kamin

import scala.util.{Failure, Success, Try}

trait Value:
  def isTrue: Boolean

case class IntegerValue(value: Int) extends Value:
  override def isTrue: Boolean = value != 0

  override def toString: String = value.toString

object IntegerValue:
  val intTrue = 1
  val intFalse = 0
  val True: IntegerValue = IntegerValue(intTrue)
  val False: IntegerValue = IntegerValue(intFalse)
  val Zero: IntegerValue = IntegerValue(0)
  implicit def intToIntegerValue(i: Int): IntegerValue = IntegerValue(i)

trait IntegerValueReader extends Reader:
  override def read(input: String): Either[String, Value] =
    Try(input.toInt) match {
      case Success(number) => Right(IntegerValue(number))
      case Failure(_) => super.read(input)
    }

given Arithmetic[IntegerValue, IntegerValue] with
  override def addition(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(IntegerValue(operand1.value + operand2.value))

  override def subtraction(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(IntegerValue(operand1.value - operand2.value))

  override def multiplication(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(IntegerValue(operand1.value * operand2.value))

  override def division(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    if operand2.value != 0 then
      Right(IntegerValue(operand1.value / operand2.value))
    else
      cannotDivideWithZero

given Relational[IntegerValue, IntegerValue] with
  override def equal(operand1: IntegerValue, operand2: IntegerValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(if operand1.value == operand2.value then booleanDefinition.trueValue else booleanDefinition.falseValue)

  override def greaterThan(operand1: IntegerValue, operand2: IntegerValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(if operand1.value > operand2.value then booleanDefinition.trueValue else booleanDefinition.falseValue)

  override def lessThan(operand1: IntegerValue, operand2: IntegerValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(if operand1.value < operand2.value then booleanDefinition.trueValue else booleanDefinition.falseValue)
