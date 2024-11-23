package kamin.apl

import kamin.ExpressionNode

case class MaximumExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class OrExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class AndExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class AdditionReductionExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class SubtractionReductionExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class MultiplicationReductionExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class DivisionReductionExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class MaximumReductionExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class OrReductionExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class AndReductionExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class CompressionExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class ShapeExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class RavelingExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class RestructuringExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class CatenationExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class IndexGenerationExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class TranspositionExpressionNode(operand: ExpressionNode) extends ExpressionNode
case class SubscriptingExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode

 
