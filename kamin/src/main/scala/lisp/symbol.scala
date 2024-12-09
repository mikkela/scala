package kamin.lisp

import kamin.{BooleanDefinition, Relational, Value}

case class SymbolValue(value: String) extends Value:
  override def isTrue: Boolean = this == SymbolValue.T
  override def toString: String = value

object SymbolValue:
  def T: SymbolValue = SymbolValue("T")


given Relational[SymbolValue, SymbolValue] with
  override def equal(operand1: SymbolValue, operand2: SymbolValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(if operand1.value == operand2.value then booleanDefinition.trueValue else booleanDefinition.falseValue)

  override def greaterThan(operand1: SymbolValue, operand2: SymbolValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(booleanDefinition.falseValue)

  override def lessThan(operand1: SymbolValue, operand2: SymbolValue)(using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(booleanDefinition.falseValue)