package kamin.scheme

import kamin.ExpressionNode

case class LambdaExpressionNode(arguments: Seq[String], expression: ExpressionNode) extends ExpressionNode
case class ValueOpExpressionNode(valueOp: String) extends ExpressionNode
case class ExpressionListExpressionNode(expressions: Seq[ExpressionNode]) extends ExpressionNode