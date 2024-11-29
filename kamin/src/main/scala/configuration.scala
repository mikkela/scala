package kamin

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

    ArithmeticRegistry.clear()
    ArithmeticRegistry.register(classOf[IntegerValue], classOf[IntegerValue], IntegerArithmetic)

    RelationalRegistry.clear()
    RelationalRegistry.register(classOf[IntegerValue], classOf[IntegerValue], IntegerRelational)
