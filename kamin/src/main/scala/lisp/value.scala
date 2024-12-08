package kamin.lisp

import kamin.Value

case class SymbolValue(value: String) extends Value:
  override def isTrue: Boolean = this == SymbolValue.T
  override def toString: String = value

object SymbolValue:
  def T: SymbolValue = SymbolValue("T")

case class ListValue(value: List[Value]) extends Value:
  override def isTrue: Boolean = this != ListValue.nil
  override def toString: String = value.mkString("(", " ", ")")

object ListValue:
  def nil: ListValue = ListValue(List.empty)
