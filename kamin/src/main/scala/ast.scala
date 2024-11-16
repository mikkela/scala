trait Node

trait InputNode extends Node

case class FunctionDefinitionNode(function: String, arguments: Seq[String], expression: ExpressionNode)
  extends InputNode

sealed trait ExpressionNode extends InputNode

case class IntegerExpressionNode(integerValue: Int) extends ExpressionNode

case class VariableExpressionNode(variableExpression: String) extends ExpressionNode

case class IfExpressionNode( testExpression: ExpressionNode,
                                consequenceExpression: ExpressionNode,
                                alternativeExpression: ExpressionNode) extends ExpressionNode

case class WhileExpressionNode(testExpression: ExpressionNode,
                              bodyExpression: ExpressionNode) extends ExpressionNode

case class SetExpressionNode(variable: String,
                                value: ExpressionNode) extends ExpressionNode

case class BeginExpressionNode(expressions: Seq[ExpressionNode]) extends ExpressionNode

case class AdditionExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class SubtractionExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class MultiplicationExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class DivisionExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class EqualityExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class LessThanExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class GreaterThanExpressionNode(operand1: ExpressionNode, operand2: ExpressionNode) extends ExpressionNode
case class PrintExpressionNode(argument: ExpressionNode) extends ExpressionNode

case class FunctionCallExpressionNode(function: String, expressions: Seq[ExpressionNode]) extends ExpressionNode


