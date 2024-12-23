package kamin.scheme

import kamin.{BooleanDefinition, Environment, EnvironmentFrame, ExpressionEvaluator, ExpressionNode, FunctionDefinitionTable, IntegerValue, IntegerValueExpressionNode, PlusToken, Reader, Value, evaluateParameters, given_ExpressionEvaluator_ExpressionNode}
import kamin.lisp.{ListValue, SExpressionNode, SymbolExpressionNode, SymbolValue}

def evaluateClosure(closure: ClosureValue, arguments: Seq[Value], environment: Environment, functionDefinitionTable: FunctionDefinitionTable, reader: Reader, booleanDefinition: BooleanDefinition): Either[String, Value] =
  if (closure.lambda.args.length != arguments.length) then return Left("Unmatched number of arguments")

  closure.lambda.args.zip(arguments).foreach((key, value) => closure.environment.set(key, value))
  closure.lambda.body.evaluateExpression(using closure.environment)(using functionDefinitionTable)(using reader)(using booleanDefinition)


private def invalidOperartionArity(operation: String, expectedArity: Int): Left[String, Nothing] =
  Left(s"$operation requires $expectedArity arguments")

def evaluateValueOperation(operation: String, arguments: Seq[Value])(using booleanDefinition: BooleanDefinition): Either[String, Value] =
  operation match
    case "+" =>
      if (arguments.length != 2)
        invalidOperartionArity(operation, 2)
      else
        kamin.addition(arguments.head, arguments(1))
    case "-" =>
      if (arguments.length != 2)
        invalidOperartionArity(operation, 2)
      else
        kamin.subtraction(arguments.head, arguments(1))
    case "*" =>
      if (arguments.length != 2)
        invalidOperartionArity(operation, 2)
      else
        kamin.multiplication(arguments.head, arguments(1))
    case "/" =>
      if (arguments.length != 2)
        invalidOperartionArity(operation, 2)
      else
        kamin.division(arguments.head, arguments(1))
    case "=" =>
      if (arguments.length != 2)
        invalidOperartionArity(operation, 2)
      else
        kamin.equal(arguments.head, arguments(1))
    case "<" =>
      if (arguments.length != 2)
        invalidOperartionArity(operation, 2)
      else
        kamin.lessThan(arguments.head, arguments(1))
    case ">" =>
      if (arguments.length != 2)
        invalidOperartionArity(operation, 2)
      else
        kamin.greaterThan(arguments.head, arguments(1))
    case "cons" =>
      if (arguments.length != 2)
        invalidOperartionArity(operation, 2)
      else
        kamin.lisp.cons(arguments)
    case "car" =>
      if (arguments.length != 1)
        invalidOperartionArity(operation, 1)
      else
        kamin.lisp.car(arguments)
    case "cdr" =>
      if (arguments.length != 1)
        invalidOperartionArity(operation, 1)
      else
        kamin.lisp.cdr(arguments)
    case "number?" =>
      if (arguments.length != 1)
        invalidOperartionArity(operation, 1)
      else
        kamin.lisp.numberTest(arguments)
    case "symbol?" =>
      if (arguments.length != 1)
        invalidOperartionArity(operation, 1)
      else
        kamin.lisp.symbolTest(arguments)
    case "list?" =>
      if (arguments.length != 1)
        invalidOperartionArity(operation, 1)
      else
        kamin.lisp.listTest(arguments)
    case "null?" =>
      if (arguments.length != 1)
        invalidOperartionArity(operation, 1)
      else
        kamin.lisp.nullTest(arguments)
    case "closure?" =>
      if (arguments.length != 1)
        invalidOperartionArity(operation, 1)
      else
        closureTest(arguments)
    case "primop?" =>
      if (arguments.length != 1)
        invalidOperartionArity(operation, 1)
      else
        primopTest(arguments)
    case _ => Left(s"Unknown operation: $operation")

given ExpressionEvaluator[ExpressionListExpressionNode] with
  extension (t: ExpressionListExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                (using functionDefinitionTable: FunctionDefinitionTable)
                                                                (using reader: Reader)
                                                                (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(t.expressions, environment, functionDefinitionTable, reader, booleanDefinition) match
      case Left(error) => Left(error)
      case Right(values) =>
        values.head match
          case value: ClosureValue => evaluateClosure(value, values.tail, environment, functionDefinitionTable, reader, booleanDefinition)
          case value: ValueOperationValue => evaluateValueOperation(value.operation, values.tail)
          case value if values.tail.isEmpty => Right(value)
          case _ => Left("Invalid list of expressions")

given ExpressionEvaluator[ValueOpExpressionNode] with
  extension (t: ValueOpExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader)
                                                                       (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(ValueOperationValue(t.valueOp))

given ExpressionEvaluator[LambdaExpressionNode] with
  extension (t: LambdaExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                      (using functionDefinitionTable: FunctionDefinitionTable)
                                                                      (using reader: Reader)
                                                                      (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    Right(ClosureValue(Lambda(t.arguments, t.expression), EnvironmentFrame(environment, t.arguments)))

def closureTest(arguments: Seq[Value]): Either[String, Value] =
  arguments match
    case List(_: ClosureValue) => Right(SymbolValue.T)
    case List(_) => Right(ListValue.nil)
    case _ => Left("Invalid parameters for closure?")

def primopTest(arguments: Seq[Value]): Either[String, Value] =
  arguments match
    case List(_: ValueOperationValue) => Right(SymbolValue.T)
    case List(_) => Right(ListValue.nil)
    case _ => Left("Invalid parameters for primop?")
