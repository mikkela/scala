package kamin.apl

import kamin.ExpressionEvaluator
import kamin.IntegerValue

object RegistriesSetup:
  def initialize(): Unit =
    kamin.RegistriesSetup.initialize()

    kamin.ExpressionEvaluatorRegistry.register(classOf[VectorValueExpressionNode], summon[ExpressionEvaluator[VectorValueExpressionNode]])
    kamin.ArithmeticRegistry.register(classOf[VectorValue], classOf[IntegerValue], VectorIntegerArithmetic)
    kamin.ArithmeticRegistry.register(classOf[IntegerValue], classOf[VectorValue], IntegerVectorArithmetic)
    kamin.ArithmeticRegistry.register(classOf[VectorValue], classOf[VectorValue], VectorVectorArithmetic)

    kamin.RelationalRegistry.register(classOf[VectorValue], classOf[IntegerValue], VectorIntegerRelational)
    kamin.RelationalRegistry.register(classOf[IntegerValue], classOf[VectorValue], IntegerVectorRelational)
    kamin.RelationalRegistry.register(classOf[VectorValue], classOf[VectorValue], VectorVectorRelational)


