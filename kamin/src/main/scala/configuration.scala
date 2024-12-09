package kamin

import kamin.apl.{MatrixValue, VectorValue}
import kamin.apl.given_Arithmetic_VectorValue_IntegerValue
import kamin.apl.given_Arithmetic_IntegerValue_VectorValue
import kamin.apl.given_Arithmetic_VectorValue_VectorValue
import kamin.apl.given_Arithmetic_IntegerValue_MatrixValue
import kamin.apl.given_Arithmetic_MatrixValue_IntegerValue
import kamin.apl.given_Arithmetic_MatrixValue_MatrixValue
import kamin.apl.given_Relational_VectorValue_IntegerValue
import kamin.apl.given_Relational_IntegerValue_VectorValue
import kamin.apl.given_Relational_VectorValue_VectorValue
import kamin.apl.given_Relational_IntegerValue_MatrixValue
import kamin.apl.given_Relational_MatrixValue_IntegerValue
import kamin.apl.given_Relational_MatrixValue_MatrixValue
import kamin.lisp.given_Relational_SymbolValue_SymbolValue
import kamin.lisp.given_Relational_ListValue_ListValue
import kamin.lisp.{ListValue, SymbolValue}

object RegistriesSetup:
  def initialize(): Unit =
    ExpressionEvaluatorRegistry.clear()
    ExpressionEvaluatorRegistry.register(classOf[IntegerValueExpressionNode], summon[ExpressionEvaluator[IntegerValueExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[VariableExpressionNode], summon[ExpressionEvaluator[VariableExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[IfExpressionNode], summon[ExpressionEvaluator[IfExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[SetExpressionNode], summon[ExpressionEvaluator[SetExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[WhileExpressionNode], summon[ExpressionEvaluator[WhileExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[BeginExpressionNode], summon[ExpressionEvaluator[BeginExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[FunctionCallExpressionNode], summon[ExpressionEvaluator[FunctionCallExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[AdditionExpressionNode], summon[ExpressionEvaluator[AdditionExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[SubtractionExpressionNode], summon[ExpressionEvaluator[SubtractionExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[MultiplicationExpressionNode], summon[ExpressionEvaluator[MultiplicationExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[DivisionExpressionNode], summon[ExpressionEvaluator[DivisionExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[EqualityExpressionNode], summon[ExpressionEvaluator[EqualityExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[LessThanExpressionNode], summon[ExpressionEvaluator[LessThanExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[GreaterThanExpressionNode], summon[ExpressionEvaluator[GreaterThanExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[PrintExpressionNode], summon[ExpressionEvaluator[PrintExpressionNode]])
    ExpressionEvaluatorRegistry.register(classOf[ReadExpressionNode], summon[ExpressionEvaluator[ReadExpressionNode]])

given ArithmeticDispatcher =
  Dispatcher.create[IntegerValue, IntegerValue, Arithmetic]
    .orElse(Dispatcher.create[IntegerValue, MatrixValue, Arithmetic])
    .orElse(Dispatcher.create[MatrixValue, IntegerValue, Arithmetic])
    .orElse(Dispatcher.create[MatrixValue, MatrixValue, Arithmetic])
    .orElse(Dispatcher.create[VectorValue, IntegerValue, Arithmetic])
    .orElse(Dispatcher.create[IntegerValue, VectorValue, Arithmetic])
    .orElse(Dispatcher.create[VectorValue, VectorValue, Arithmetic])

given RelationalDispatcher =
  Dispatcher.create[IntegerValue, IntegerValue, Relational]
    .orElse(Dispatcher.create[IntegerValue, MatrixValue, Relational])
    .orElse(Dispatcher.create[MatrixValue, IntegerValue, Relational])
    .orElse(Dispatcher.create[MatrixValue, MatrixValue, Relational])
    .orElse(Dispatcher.create[VectorValue, IntegerValue, Relational])
    .orElse(Dispatcher.create[IntegerValue, VectorValue, Relational])
    .orElse(Dispatcher.create[VectorValue, VectorValue, Relational])
    .orElse(Dispatcher.create[SymbolValue, SymbolValue, Relational])
    .orElse(Dispatcher.create[ListValue, ListValue, Relational])
