package kamin.lisp

import kamin.{Environment, ExpressionEvaluator, FunctionDefinitionTable, IntegerValue, IntegerValueExpressionNode, Reader, Relational, Value, evaluateParameters, given_ExpressionEvaluator_IntegerValueExpressionNode}

given ExpressionEvaluator[SExpressionNode] with
  extension (t: SExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader)
                                                                          (using falseValue: Value): Either[String, Value] =
    t.expression match
      case integer: IntegerValueExpressionNode =>
        integer.evaluateExpression(using environment)(using functionDefinitionTable)(using reader)(using falseValue)
      case symbol: SymbolExpressionNode =>
        Right(SymbolValue(symbol.symbol))
      case list: Seq[SExpressionNode] =>
        val evaluations = list.map(_.evaluateExpression(using environment)(using functionDefinitionTable)(using reader)(using falseValue))
        evaluations.foldRight[Either[String, List[Value]]](Right(Nil)){
          case (Right(value), Right(acc)) => Right(value::acc)
          case (Left(err), _) => Left(err)
          case (_, Left(err)) => Left(err)
        }.map(values => ListValue(values))

given ExpressionEvaluator[ConsExpressionNode] with
  extension (t: ConsExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                      (using functionDefinitionTable: FunctionDefinitionTable)
                                                                      (using reader: Reader)
                                                                      (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, falseValue).flatMap {
      case List(operand1, operand2: ListValue) => Right(ListValue(operand1::operand2.value))
      case _ => Left("Invalid parameters for cons. Requires a value and a list")
    }

given ExpressionEvaluator[CarExpressionNode] with
  extension (t: CarExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                   (using functionDefinitionTable: FunctionDefinitionTable)
                                                                   (using reader: Reader)
                                                                   (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, falseValue).flatMap {
      case List(operand: ListValue) if operand != ListValue.nil => Right(operand.value.head)
      case _ => Left("Invalid parameters for car. Requires a non-empty list")
    }

given ExpressionEvaluator[CdrExpressionNode] with
  extension (t: CdrExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                  (using functionDefinitionTable: FunctionDefinitionTable)
                                                                  (using reader: Reader)
                                                                  (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, falseValue).flatMap {
      case List(operand: ListValue) if operand.value.length > 1 => Right(ListValue(operand.value.tail))
      case List(operand: ListValue) if operand.value.length == 1 => Right(ListValue.nil)
      case _ => Left("Invalid parameters for cdr. Requires a non-empty list")
    }

given ExpressionEvaluator[NumberTestExpressionNode] with
  extension (t: NumberTestExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                  (using functionDefinitionTable: FunctionDefinitionTable)
                                                                  (using reader: Reader)
                                                                  (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, falseValue).flatMap {
      case List(_: IntegerValue) => Right(SymbolValue.T)
      case List(_)  => Right(ListValue.nil)
      case _ => Left("Invalid parameters for number?")
    }

given ExpressionEvaluator[SymbolTestExpressionNode] with
  extension (t: SymbolTestExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                                                         (using reader: Reader)
                                                                         (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, falseValue).flatMap {
      case List(_: SymbolValue) => Right(SymbolValue.T)
      case List(_) => Right(ListValue.nil)
      case _ => Left("Invalid parameters for symbol?")
    }

given ExpressionEvaluator[ListTestExpressionNode] with
  extension (t: ListTestExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                                                         (using reader: Reader)
                                                                         (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, falseValue).flatMap {
      case List(operand: ListValue) if operand != ListValue.nil => Right(SymbolValue.T)
      case List(_) => Right(ListValue.nil)
      case _ => Left("Invalid parameters for list?")
    }

given ExpressionEvaluator[NullTestExpressionNode] with
  extension (t: NullTestExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader)
                                                                       (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, falseValue).flatMap {
      case List(operand: ListValue) if operand == ListValue.nil => Right(SymbolValue.T)
      case List(_) => Right(ListValue.nil)
      case _ => Left("Invalid parameters for null?")
    }

object IntegerRelational extends Relational[IntegerValue, IntegerValue]:
  override def equal(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(if operand1.value == operand2.value then SymbolValue.T else ListValue.nil)

  override def greaterThan(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(if operand1.value > operand2.value then SymbolValue.T else ListValue.nil)

  override def lessThan(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(if operand1.value < operand2.value then SymbolValue.T else ListValue.nil)

object SymbolRelational extends Relational[SymbolValue, SymbolValue]:
  override def equal(operand1: SymbolValue, operand2: SymbolValue): Either[String, Value] =
    Right(if operand1.value == operand2.value then SymbolValue.T else ListValue.nil)

  override def greaterThan(operand1: SymbolValue, operand2: SymbolValue): Either[String, Value] =
    Right(ListValue.nil)

  override def lessThan(operand1: SymbolValue, operand2: SymbolValue): Either[String, Value] =
    Right(ListValue.nil)

object ListRelational extends Relational[ListValue, ListValue]:
  override def equal(operand1: ListValue, operand2: ListValue): Either[String, Value] =
    Right(if operand1 == ListValue.nil && operand2 == ListValue.nil then SymbolValue.T else ListValue.nil)

  override def greaterThan(operand1: ListValue, operand2: ListValue): Either[String, Value] =
    Right(ListValue.nil)

  override def lessThan(operand1: ListValue, operand2: ListValue): Either[String, Value] =
    Right(ListValue.nil)


