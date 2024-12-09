package kamin.lisp

import kamin.{Relational, Value}

case class ListValue(value: List[Value]) extends Value:
  override def isTrue: Boolean = this != ListValue.nil
  override def toString: String = value.mkString("(", " ", ")")

object ListValue:
  def nil: ListValue = ListValue(List.empty)

given Relational[ListValue, ListValue] with
  override def equal(operand1: ListValue, operand2: ListValue): Either[String, Value] =
    Right(if operand1 == ListValue.nil && operand2 == ListValue.nil then SymbolValue.T else ListValue.nil)

  override def greaterThan(operand1: ListValue, operand2: ListValue): Either[String, Value] =
    Right(ListValue.nil)

  override def lessThan(operand1: ListValue, operand2: ListValue): Either[String, Value] =
    Right(ListValue.nil)