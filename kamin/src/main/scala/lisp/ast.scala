package kamin.lisp

import kamin.{ExpressionNode, IntegerValueExpressionNode, Value}

case class SymbolExpressionNode(symbol: String) extends ExpressionNode
case class SExpressionNode(expression: IntegerValueExpressionNode | SymbolExpressionNode | Seq[SExpressionNode]) extends ExpressionNode
case class ConsExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class CarExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class CdrExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class NumberTestExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class SymbolTestExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class ListTestExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class NullTestExpressionNode(operand: ExpressionNode) extends ExpressionNode
