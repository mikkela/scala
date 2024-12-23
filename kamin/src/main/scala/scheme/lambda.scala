package kamin.scheme

import kamin.{ExpressionNode, Environment, Value}
import kamin.lisp.ListValue

case class Lambda(args: Seq[String], body: ExpressionNode)

case class ClosureValue(lambda: Lambda, environment: Environment) extends Value:
  override def isTrue: Boolean = false
  override def toString: String = "<closure>"
