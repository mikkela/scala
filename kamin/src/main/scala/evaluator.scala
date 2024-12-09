package kamin

import scala.annotation.tailrec
import scala.io.StdIn
import scala.collection.mutable

def cannotDivideWithZero = Left("Cannot divide with zero")

trait Evaluator:
  def evaluate(input: String): String

trait ExpressionEvaluator[T <: ExpressionNode]:
  extension (t: T) def evaluateExpression(using environment: Environment)
                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                         (using reader: Reader)
                                         (using falseValue: Value): Either[String, Value]

trait Arithmetic[T1 <: Value, T2 <: Value]:
  def addition(operand1: T1, operand2: T2): Either[String, Value]
  def subtraction(operand1: T1, operand2: T2): Either[String, Value]
  def multiplication(operand1: T1, operand2: T2): Either[String, Value]
  def division(operand1: T1, operand2: T2): Either[String, Value]

trait Relational[T1 <: Value, T2 <: Value]:
  def equal(operand1: T1, operand2: T2): Either[String, Value]
  def lessThan(operand1: T1, operand2: T2): Either[String, Value]
  def greaterThan(operand1: T1, operand2: T2): Either[String, Value]

type ArithmeticDispatcher = Dispatcher[Arithmetic]
type RelationalDispatcher = Dispatcher[Relational]

trait Reader:
  def read(input: String): Either[String, Value] =
    Left(s"'$input' is not understood")

private def unrecognizedName(name: String) : String = s"$name is not recognized"

given ExpressionEvaluator[ExpressionNode] with
  extension (t: ExpressionNode) override def evaluateExpression(using environment: Environment)
                                                               (using functionDefinitionTable: FunctionDefinitionTable)
                                                               (using reader: Reader)
                                                               (using falseValue: Value): Either[String, Value]=

  ExpressionEvaluatorRegistry.get(t.getClass) match
    case Some(evaluator) => evaluator.asInstanceOf[ExpressionEvaluator[t.type]].evaluateExpression(t)(using environment)(using functionDefinitionTable)(using reader)
    case None            => Left(s"No evaluator registered for ${t.getClass.getName}")


given ExpressionEvaluator[IntegerValueExpressionNode] with
  extension (t: IntegerValueExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                           (using functionDefinitionTable: FunctionDefinitionTable)
                                                                           (using reader: Reader)
                                                                           (using falseValue: Value): Either[String, Value] =
    Right(t.integerValue)

given ExpressionEvaluator[VariableExpressionNode] with
  extension (t: VariableExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader)
                                                                       (using falseValue: Value): Either[String, Value] =
    environment.get(t.variableExpression) match
      case Some(value) => Right(value)
      case None => Left(unrecognizedName(t.variableExpression))

given ExpressionEvaluator[IfExpressionNode] with
  extension (t: IfExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                 (using functionDefinitionTable: FunctionDefinitionTable)
                                                                 (using reader: Reader)
                                                                 (using falseValue: Value): Either[String, Value] =
    t.testExpression.evaluateExpression match
      case Left(error) => Left(error)
      case Right(test) if test.isTrue => t.consequenceExpression.evaluateExpression
      case Right(_) => t.alternativeExpression.evaluateExpression

given ExpressionEvaluator[SetExpressionNode] with
  extension (t: SetExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                  (using functionDefinitionTable: FunctionDefinitionTable)
                                                                  (using reader: Reader)
                                                                  (using falseValue: Value): Either[String, Value] =
    t.value.evaluateExpression match
      case Left(value) => Left(value)
      case Right(value) =>
        environment.set(t.variable, value)
        Right(value)

given ExpressionEvaluator[WhileExpressionNode] with
  extension (t: WhileExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                    (using functionDefinitionTable: FunctionDefinitionTable)
                                                                    (using reader: Reader)
                                                                    (using falseValue: Value): Either[String, Value] =
    @tailrec
    def evaluateLoop(): Either[String, Value] =
      t.testExpression.evaluateExpression match
        case Left(error) => Left(error)
        case Right(test) if !test.isTrue => Right(falseValue)
        case Right(_) =>
          t.bodyExpression.evaluateExpression match
            case Left(error) => Left(error)
            case Right(_) => evaluateLoop() // Recur to continue the loop

    evaluateLoop()

given ExpressionEvaluator[BeginExpressionNode] with
  extension (t: BeginExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                    (using functionDefinitionTable: FunctionDefinitionTable)
                                                                    (using reader: Reader)
                                                                    (using falseValue: Value): Either[String, Value] =
    t.expressions.foldLeft[Either[String, Value]](Right(falseValue)) { (acc, expr) =>
      acc match
        case Left(error) => Left(error)
        case Right(_) => expr.evaluateExpression
    }

private def undefinedFunctionName(name: String): Left[String, Nothing] =
  Left(s"$name is not recognized as a function")

private def invalidFunctionArity(function: String, expectedArity: Int): Left[String, Nothing] =
  Left(s"$function requires $expectedArity arguments")

private def evaluateParameters(parameters: Seq[ExpressionNode],
                               environment: Environment,
                               functionDefinitionTable: FunctionDefinitionTable,
                               reader: Reader,
                               falseValue: Value
                              ): Either[String, List[Value]] =
  parameters.foldLeft(Right(List.empty[Value]): Either[String, List[Value]]) { (acc, p) =>
    acc match
      case Left(error) => Left(error) // If there's already an error, keep it
      case Right(params) =>
        p.evaluateExpression(using environment)(using functionDefinitionTable)(using reader)(using falseValue) match
          case Left(error) => Left(error) // Stop and return the error if evaluation fails
          case Right(result) => Right(params :+ result) // Append result to the list if successful
  }
given ExpressionEvaluator[FunctionCallExpressionNode] with
  extension (t: FunctionCallExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                           (using functionDefinitionTable: FunctionDefinitionTable)
                                                                           (using reader: Reader)
                                                                           (using falseValue: Value): Either[String, Value] =

    val parameters = evaluateParameters(t.expressions, environment, functionDefinitionTable, reader, falseValue)

    functionDefinitionTable.lookupFunctionDefinition(t.function) match
      case None => undefinedFunctionName(t.function)
      case Some(functionDefinition) =>
        parameters match
          case Right(params) if params.length == functionDefinition.arguments.length =>
            environment.openScope(functionDefinition.arguments)
            functionDefinition.arguments.zip(params).foreach((a, p) => environment.set(a, p))

            functionDefinition.expression.evaluateExpression(using environment)(using functionDefinitionTable) match
              case Left(error) => Left(error)
              case Right(result) =>
                environment.closeScope()
                Right(result)
          case Right(params) => invalidFunctionArity(functionDefinition.function, functionDefinition.arguments.length)
          case Left(error) => Left(error)

def arithmeticOperation[T1 <: Value, T2 <: Value](
                                                   v1: T1,
                                                   v2: T2,
                                                   op: Arithmetic[T1, T2] => (T1, T2) => Either[String, Value],
                                                   opName: String
                                                 )(using dispatcher: ArithmeticDispatcher): Either[String, Value] =
  dispatcher.dispatch(v1, v2) match
    case Some(arithmetic: Arithmetic[T1, T2] @unchecked) =>
      op(arithmetic)(v1, v2)
    case None =>
      Left(s"$opName is not supported for ${v1.getClass} and ${v2.getClass}")

def relationalOperation[T1 <: Value, T2 <: Value](
                                                   v1: T1,
                                                   v2: T2,
                                                   op: Relational[T1, T2] => (T1, T2) => Either[String, Value],
                                                   opName: String
                                                 )(using dispatcher: RelationalDispatcher): Either[String, Value] =
  dispatcher.dispatch(v1, v2) match
    case Some(relational: Relational[T1, T2] @unchecked) =>
      op(relational)(v1, v2)
    case None =>
      Left(s"$opName is not supported for ${v1.getClass} and ${v2.getClass}")


given ExpressionEvaluator[AdditionExpressionNode] with
  extension (t: AdditionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader)
                                                                       (using falseValue: Value): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, falseValue) match
      case Left(error) => Left(error)
      case Right(params) => arithmeticOperation(params.head, params(1), _.addition, "Addition")

given ExpressionEvaluator[SubtractionExpressionNode] with
  extension (t: SubtractionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader)
                                                                          (using falseValue: Value): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, falseValue) match
      case Left(error) => Left(error)
      case Right(params) => arithmeticOperation(params.head, params(1), _.subtraction, "Subtraction")

given ExpressionEvaluator[MultiplicationExpressionNode] with
  extension (t: MultiplicationExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                             (using functionDefinitionTable: FunctionDefinitionTable)
                                                                             (using reader: Reader)
                                                                             (using falseValue: Value): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, falseValue) match
      case Left(error) => Left(error)
      case Right(params) => arithmeticOperation(params.head, params(1), _.multiplication, "Multiplication")

given ExpressionEvaluator[DivisionExpressionNode] with
  extension (t: DivisionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader)
                                                                       (using falseValue: Value): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, falseValue) match
      case Left(error) => Left(error)
      case Right(params) => arithmeticOperation(params.head, params(1), _.division, "Division")

given ExpressionEvaluator[EqualityExpressionNode] with
  extension (t: EqualityExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader)
                                                                       (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, falseValue) match
      case Left(error) => Left(error)
      case Right(params) => relationalOperation(params.head, params(1), _.equal, "Equal")

given ExpressionEvaluator[LessThanExpressionNode] with
  extension (t: LessThanExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader)
                                                                       (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, falseValue) match
      case Left(error) => Left(error)
      case Right(params) => relationalOperation(params.head, params(1), _.lessThan, "Less Than")

given ExpressionEvaluator[GreaterThanExpressionNode] with
  extension (t: GreaterThanExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader)
                                                                          (using falseValue: Value): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader, falseValue) match
      case Left(error) => Left(error)
      case Right(params) => relationalOperation(params.head, params(1), _.greaterThan, "Greater Than")

given ExpressionEvaluator[PrintExpressionNode] with
  extension (t: PrintExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                    (using functionDefinitionTable: FunctionDefinitionTable)
                                                                    (using reader: Reader)
                                                                    (using falseValue: Value): Either[String, Value] =

    evaluateParameters(Seq(t.argument), environment, functionDefinitionTable, reader, falseValue) match
      case Left(error) => Left(error)
      case Right(params) =>
        println(params.head)
        Right(params.head)

given ExpressionEvaluator[ReadExpressionNode] with
  extension (t: ReadExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                   (using functionDefinitionTable: FunctionDefinitionTable)
                                                                   (using reader: Reader)
                                                                   (using falseValue: Value): Either[String, Value] =
    val input = StdIn.readLine()

    reader.read(input)

class TypeRegistry[K, V]:
  private val registry = mutable.Map[K, V]()

  def register(key: K, value: V): Unit =
    registry(key) = value

  def get(key: K): Option[V] = registry.get(key)

  def clear(): Unit = registry.clear()

  def unregister(key: K): Unit = registry.remove(key)

object ExpressionEvaluatorRegistry:
  private val registry = new TypeRegistry[Class[? <: ExpressionNode], ExpressionEvaluator[? <: ExpressionNode]]()

  def register[T <: ExpressionNode](cls: Class[T], evaluator: ExpressionEvaluator[T]): Unit =
    registry.register(cls, evaluator.asInstanceOf[ExpressionEvaluator[? <: ExpressionNode]])

  def get[T <: ExpressionNode](cls: Class[T]): Option[ExpressionEvaluator[T]] =
    registry.get(cls).asInstanceOf[Option[ExpressionEvaluator[T]]]

  def clear(): Unit = registry.clear()
