package kamin.lisp

import kamin.{Relational, Value}

case class SymbolValue(value: String) extends Value:
  override def isTrue: Boolean = this == SymbolValue.T
  override def toString: String = value

object SymbolValue:
  def T: SymbolValue = SymbolValue("T")


given Relational[SymbolValue, SymbolValue] with
  override def equal(operand1: SymbolValue, operand2: SymbolValue): Either[String, Value] =
    Right(if operand1.value == operand2.value then SymbolValue.T else ListValue.nil)

  override def greaterThan(operand1: SymbolValue, operand2: SymbolValue): Either[String, Value] =
    Right(ListValue.nil)

  override def lessThan(operand1: SymbolValue, operand2: SymbolValue): Either[String, Value] =
    Right(ListValue.nil)