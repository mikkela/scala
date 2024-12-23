package kamin.lisp

import kamin.{BooleanDefinition, Environment, ExpressionEvaluator, FunctionDefinitionTable, IntegerValue, IntegerValueExpressionNode, Reader, Relational, Value, evaluateParameters, given_ExpressionEvaluator_IntegerValueExpressionNode}

given ExpressionEvaluator[SExpressionNode] with
  extension (t: SExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader)
                                                                          (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    t.expression match
      case integer: IntegerValueExpressionNode =>
        kamin.evaluate(integer)
      case symbol: SymbolExpressionNode =>
        Right(SymbolValue(symbol.symbol))
      case list: Seq[SExpressionNode] => {
        val evaluations = list.map(_.evaluateExpression(using environment)(using functionDefinitionTable)(using reader)(using booleanDefinition))
        evaluations.foldRight[Either[String, List[Value]]](Right(Nil)) {
          case (Right(value), Right(acc)) => Right(value :: acc)
          case (Left(err), _) => Left(err)
          case (_, Left(err)) => Left(err)
        }.map(values => ListValue(values))
      }

def cons(arguments: Seq[Value]): Either[String, Value] =
  arguments match
    case Seq(operand1, operand2: ListValue) => Right(ListValue(operand1 :: operand2.value))
    case _ => Left("Invalid parameters for cons. Requires a value and a list")

given ExpressionEvaluator[ConsExpressionNode] with
  extension (t: ConsExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                      (using functionDefinitionTable: FunctionDefinitionTable)
                                                                      (using reader: Reader)
                                                                      (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      cons(_)
    }

def car(arguments: Seq[Value]): Either[String, Value] =
  arguments match
    case List(operand: ListValue) if operand != ListValue.nil => Right(operand.value.head)
    case _ => Left("Invalid parameters for car. Requires a non-empty list")

given ExpressionEvaluator[CarExpressionNode] with
  extension (t: CarExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                   (using functionDefinitionTable: FunctionDefinitionTable)
                                                                   (using reader: Reader)
                                                                   (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      car(_)
    }

def cdr(arguments: Seq[Value]): Either[String, Value] =
  arguments match
    case List(operand: ListValue) if operand.value.length > 1 => Right(ListValue(operand.value.tail))
    case List(operand: ListValue) if operand.value.length == 1 => Right(ListValue.nil)
    case _ => Left("Invalid parameters for cdr. Requires a non-empty list")
      
given ExpressionEvaluator[CdrExpressionNode] with
  extension (t: CdrExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                  (using functionDefinitionTable: FunctionDefinitionTable)
                                                                  (using reader: Reader)
                                                                  (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      cdr(_)
    }

def numberTest(arguments: Seq[Value]): Either[String, Value] =
  arguments match
    case List(_: IntegerValue) => Right(SymbolValue.T)
    case List(_) => Right(ListValue.nil)
    case _ => Left("Invalid parameters for number?")
given ExpressionEvaluator[NumberTestExpressionNode] with
  extension (t: NumberTestExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                  (using functionDefinitionTable: FunctionDefinitionTable)
                                                                  (using reader: Reader)
                                                                  (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      numberTest(_)
    }

def symbolTest(arguments: Seq[Value]): Either[String, Value] =
  arguments match
    case List(_: SymbolValue) => Right(SymbolValue.T)
    case List(_) => Right(ListValue.nil)
    case _ => Left("Invalid parameters for number?")
given ExpressionEvaluator[SymbolTestExpressionNode] with
  extension (t: SymbolTestExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                                                         (using reader: Reader)
                                                                         (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      symbolTest(_)
    }

def listTest(arguments: Seq[Value]): Either[String, Value] =
  arguments match
    case List(operand: ListValue) if operand != ListValue.nil => Right(SymbolValue.T)
    case List(_) => Right(ListValue.nil)
    case _ => Left("Invalid parameters for number?")
given ExpressionEvaluator[ListTestExpressionNode] with
  extension (t: ListTestExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                                                         (using reader: Reader)
                                                                          (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      listTest(_)
    }

def nullTest(arguments: Seq[Value]): Either[String, Value] =
  arguments match
    case List(operand: ListValue) if operand == ListValue.nil => Right(SymbolValue.T)
    case List(_) => Right(ListValue.nil)
    case _ => Left("Invalid parameters for number?")
given ExpressionEvaluator[NullTestExpressionNode] with
  extension (t: NullTestExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader)
                                                                       (using booleanDefinition: BooleanDefinition): Either[String, Value] =
    evaluateParameters(Seq(t.operand), environment, functionDefinitionTable, reader, booleanDefinition).flatMap {
      nullTest(_)
    }




