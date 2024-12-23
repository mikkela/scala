package kamin.scheme

import kamin.Value

case class ValueOperationValue(operation: String) extends Value:
  override def isTrue: Boolean = false
  override def toString: String = operation


