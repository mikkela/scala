package kamin

import scala.annotation.tailrec
import scala.io.StdIn
import scala.collection.mutable

trait Evaluator:
  def evaluate(input: String): String

trait ExpressionEvaluator[T <: ExpressionNode]:
  extension (t: T) def evaluateExpression(using environment: Environment)
                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                         (using reader: Reader): Either[String, Value]

trait OperandValidator[T1 <: Value, T2 <: Value]:
  def validateOperands(operand1: Value, operand2: Value): Option[(T1, T2)] =
    (operand1, operand2) match
      case (op1: T1 @unchecked, op2: T2 @unchecked) => Some((op1, op2))
      case _ => None

trait Arithmetic[T1 <: Value, T2 <: Value] extends OperandValidator[T1, T2]:
  def addition(operand1: T1, operand2: T2): Either[String, Value]
  def subtraction(operand1: T1, operand2: T2): Either[String, Value]
  def multiplication(operand1: T1, operand2: T2): Either[String, Value]
  def division(operand1: T1, operand2: T2): Either[String, Value]

trait Relational[T1 <: Value, T2 <: Value] extends OperandValidator[T1, T2]:
  def equal(operand1: T1, operand2: T2): Either[String, Value]
  def lessThan(operand1: T1, operand2: T2): Either[String, Value]
  def greaterThan(operand1: T1, operand2: T2): Either[String, Value]

trait Reader:
  def read(input: String): Either[String, Value] =
    Left(s"'$input' is not understood")

private def unrecognizedName(name: String) : String = s"$name is not recognized"

given ExpressionEvaluator[ExpressionNode] with
  extension (t: ExpressionNode) override def evaluateExpression(using environment: Environment)
                                                               (using functionDefinitionTable: FunctionDefinitionTable)
                                                               (using reader: Reader): Either[String, Value]=

  ExpressionEvaluatorRegistry.get(t.getClass) match
    case Some(evaluator) => evaluator.asInstanceOf[ExpressionEvaluator[t.type]].evaluateExpression(t)(using environment)(using functionDefinitionTable)(using reader)
    case None            => Left(s"No evaluator registered for ${t.getClass.getName}")


given ExpressionEvaluator[IntegerValueExpressionNode] with
  extension (t: IntegerValueExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                           (using functionDefinitionTable: FunctionDefinitionTable)
                                                                           (using reader: Reader): Either[String, Value] =
    Right(t.integerValue)

given ExpressionEvaluator[VariableExpressionNode] with
  extension (t: VariableExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader): Either[String, Value] =
    environment.get(t.variableExpression) match
      case Some(value) => Right(value)
      case None => Left(unrecognizedName(t.variableExpression))

given ExpressionEvaluator[IfExpressionNode] with
  extension (t: IfExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                 (using functionDefinitionTable: FunctionDefinitionTable)
                                                                 (using reader: Reader): Either[String, Value] =
    t.testExpression.evaluateExpression match
      case Left(error) => Left(error)
      case Right(test) if test.isTrue => t.consequenceExpression.evaluateExpression
      case Right(_) => t.alternativeExpression.evaluateExpression

given ExpressionEvaluator[SetExpressionNode] with
  extension (t: SetExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                  (using functionDefinitionTable: FunctionDefinitionTable)
                                                                  (using reader: Reader): Either[String, Value] =
    t.value.evaluateExpression match
      case Left(value) => Left(value)
      case Right(value) =>
        environment.set(t.variable, value)
        Right(value)

given ExpressionEvaluator[WhileExpressionNode] with
  extension (t: WhileExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                    (using functionDefinitionTable: FunctionDefinitionTable)
                                                                    (using reader: Reader): Either[String, Value] =
    @tailrec
    def evaluateLoop(): Either[String, Value] =
      t.testExpression.evaluateExpression match
        case Left(error) => Left(error)
        case Right(test) if !test.isTrue => Right(IntegerValue.False)
        case Right(_) =>
          t.bodyExpression.evaluateExpression match
            case Left(error) => Left(error)
            case Right(_) => evaluateLoop() // Recur to continue the loop

    evaluateLoop()

given ExpressionEvaluator[BeginExpressionNode] with
  extension (t: BeginExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                    (using functionDefinitionTable: FunctionDefinitionTable)
                                                                    (using reader: Reader): Either[String, Value] =
    t.expressions.foldLeft[Either[String, Value]](Right(IntegerValue.False)) { (acc, expr) =>
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
                               reader: Reader
                              ): Either[String, List[Value]] =
  parameters.foldLeft(Right(List.empty[Value]): Either[String, List[Value]]) { (acc, p) =>
    acc match
      case Left(error) => Left(error) // If there's already an error, keep it
      case Right(params) =>
        p.evaluateExpression(using environment)(using functionDefinitionTable)(using reader) match
          case Left(error) => Left(error) // Stop and return the error if evaluation fails
          case Right(result) => Right(params :+ result) // Append result to the list if successful
  }
given ExpressionEvaluator[FunctionCallExpressionNode] with
  extension (t: FunctionCallExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                           (using functionDefinitionTable: FunctionDefinitionTable)
                                                                           (using reader: Reader): Either[String, Value] =

    val parameters = evaluateParameters(t.expressions, environment, functionDefinitionTable, reader)

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

def performArithmeticOperation(
                                params: Seq[Value],
                                operation: (Arithmetic[Value, Value], Value, Value) => Either[String, Value]
                              ): Either[String, Value] =
  if (params.size != 2)
    return Left("Arithmetic operations require exactly two operands")

  val cls1 = params.head.getClass
  val cls2 = params(1).getClass

  ArithmeticRegistry.get(cls1, cls2) match
    case Some(arith) =>
      arith.validateOperands(params.head, params(1)) match
        case Some((operand1, operand2)) =>
          operation(arith.asInstanceOf[Arithmetic[Value, Value]], operand1, operand2)
        case None =>
          Left(s"Type mismatch: incompatible operands for ${cls1.getName} and ${cls2.getName}")

    case None =>
      Left(s"No arithmetic registered for ${cls1.getName} and ${cls2.getName}")

given ExpressionEvaluator[AdditionExpressionNode] with
  extension (t: AdditionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => performArithmeticOperation(params, (arith, op1, op2) => arith.addition(op1, op2))

given ExpressionEvaluator[SubtractionExpressionNode] with
  extension (t: SubtractionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => performArithmeticOperation(params, (arith, op1, op2) => arith.subtraction(op1, op2))

given ExpressionEvaluator[MultiplicationExpressionNode] with
  extension (t: MultiplicationExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                             (using functionDefinitionTable: FunctionDefinitionTable)
                                                                             (using reader: Reader): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => performArithmeticOperation(params, (arith, op1, op2) => arith.multiplication(op1, op2))

given ExpressionEvaluator[DivisionExpressionNode] with
  extension (t: DivisionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => performArithmeticOperation(params, (arith, op1, op2) => arith.division(op1, op2))

def performRelationalOperation(
                                params: Seq[Value],
                                operation: (Relational[Value, Value], Value, Value) => Either[String, Value]
                              ): Either[String, Value] =
  if (params.size != 2)
    return Left("Arithmetic operations require exactly two operands")

  val cls1 = params.head.getClass
  val cls2 = params(1).getClass

  RelationalRegistry.get(cls1, cls2) match
    case Some(relational) =>
      relational.validateOperands(params.head, params(1)) match
        case Some((operand1, operand2)) =>
          operation(relational.asInstanceOf[Relational[Value, Value]], operand1, operand2)
        case None =>
          Left(s"Type mismatch: incompatible operands for ${cls1.getName} and ${cls2.getName}")

    case None =>
      Left(s"No relational registered for ${cls1.getName} and ${cls2.getName}")

given ExpressionEvaluator[EqualityExpressionNode] with
  extension (t: EqualityExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => performRelationalOperation(params, (relational, op1, op2) => relational.equal(op1, op2))

given ExpressionEvaluator[LessThanExpressionNode] with
  extension (t: LessThanExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => performRelationalOperation(params, (relational, op1, op2) => relational.lessThan(op1, op2))

given ExpressionEvaluator[GreaterThanExpressionNode] with
  extension (t: GreaterThanExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => performRelationalOperation(params, (relational, op1, op2) => relational.greaterThan(op1, op2))

given ExpressionEvaluator[PrintExpressionNode] with
  extension (t: PrintExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                    (using functionDefinitionTable: FunctionDefinitionTable)
                                                                    (using reader: Reader): Either[String, Value] =

    evaluateParameters(Seq(t.argument), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) =>
        println(params.head)
        Right(params.head)

given ExpressionEvaluator[ReadExpressionNode] with
  extension (t: ReadExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                   (using functionDefinitionTable: FunctionDefinitionTable)
                                                                   (using reader: Reader): Either[String, Value] =
    val input = StdIn.readLine()

    reader.read(input)

object IntegerArithmetic extends Arithmetic[IntegerValue, IntegerValue]:
  override def addition(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(IntegerValue(operand1.value + operand2.value))

  override def subtraction(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(IntegerValue(operand1.value - operand2.value))

  override def multiplication(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(IntegerValue(operand1.value * operand2.value))

  override def division(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(IntegerValue(operand1.value / operand2.value))

object IntegerRelational extends Relational[IntegerValue, IntegerValue]:
  override def equal(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(if operand1.value == operand2.value then IntegerValue.True else IntegerValue.False)

  override def greaterThan(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(if operand1.value > operand2.value then IntegerValue.True else IntegerValue.False)

  override def lessThan(operand1: IntegerValue, operand2: IntegerValue): Either[String, Value] =
    Right(if operand1.value < operand2.value then IntegerValue.True else IntegerValue.False)


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

type ValuePair = (Class[? <: Value], Class[? <: Value])

object ArithmeticRegistry:
  private val registry = new TypeRegistry[ValuePair, Arithmetic[?, ?]]()

  def register[T1 <: Value, T2 <: Value](cls1: Class[T1], cls2: Class[T2], arithmetic: Arithmetic[T1, T2]): Unit =
    registry.register((cls1, cls2), arithmetic.asInstanceOf[Arithmetic[?, ?]])

  def get[T1 <: Value, T2 <: Value](cls1: Class[T1], cls2: Class[T2]): Option[Arithmetic[T1, T2]] =
    registry.get((cls1, cls2)).asInstanceOf[Option[Arithmetic[T1, T2]]]

  def clear(): Unit = registry.clear()

object RelationalRegistry:
  private val registry = new TypeRegistry[ValuePair, Relational[?, ?]]()

  def register[T1 <: Value, T2 <: Value](cls1: Class[T1], cls2: Class[T2], relational: Relational[T1, T2]): Unit =
    registry.register((cls1, cls2), relational.asInstanceOf[Relational[?, ?]])

  def get[T1 <: Value, T2 <: Value](cls1: Class[T1], cls2: Class[T2]): Option[Relational[T1, T2]] =
    registry.get((cls1, cls2)).asInstanceOf[Option[Relational[T1, T2]]]

  def clear(): Unit = registry.clear()

def performOperation[Op[T1 <: Value, T2 <: Value] <: OperandValidator[T1, T2]](
                                                                                params: Seq[Value],
                                                                                registry: TypeRegistry[(Class[_ <: Value], Class[_ <: Value]), Op[Value, Value]],
                                                                                operation: (Op[Value, Value], Value, Value) => Either[String, Value],
                                                                                operationType: String
                                                                              ): Either[String, Value] =
  if (params.size != 2)
    return Left(s"$operationType operations require exactly two operands")

  val cls1 = params.head.getClass
  val cls2 = params(1).getClass

  registry.get((cls1, cls2)) match
    case Some(opHandler) =>
      opHandler.validateOperands(params.head, params(1)) match
        case Some((operand1, operand2)) =>
          operation(opHandler, operand1, operand2)
        case None =>
          Left(s"Type mismatch: incompatible operands for ${cls1.getName} and ${cls2.getName}")

    case None =>
      Left(s"No $operationType registered for ${cls1.getName} and ${cls2.getName}")