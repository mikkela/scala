import scala.annotation.tailrec
import scala.io.StdIn
import scala.reflect.TypeTest

trait Evaluator:
  def evaluate(input: String): String

trait ExpressionEvaluator[T <: ExpressionNode]:
  extension (t: T) def evaluateExpression(using environment: Environment)
                                         (using functionDefinitionTable: FunctionDefinitionTable)
                                         (using reader: Reader): Either[String, Value]

trait Dispatcher[Op[_ <: Value, _ <: Value]]:
  def dispatch(v1: Value, v2: Value): Option[Op[Value, Value]]

object Dispatcher:
  def create[T1 <: Value, T2 <: Value, Op[_ <: Value, _ <: Value]](using
                                                                   tt1: TypeTest[Value, T1],
                                                                   tt2: TypeTest[Value, T2],
                                                                   operation: Op[T1, T2]
                                                                  ): Dispatcher[Op] =
    new Dispatcher[Op]:
      def dispatch(v1: Value, v2: Value): Option[Op[Value, Value]] =
        for
          t1 <- tt1.unapply(v1)
          t2 <- tt2.unapply(v2)
        yield operation.asInstanceOf[Op[Value, Value]]

trait Arithmetic[T1 <: Value, T2 <: Value]:
  def addition(operand1: T1, operand2: T2): Value
  def subtraction(operand1: T1, operand2: T2): Value
  def multiplication(operand1: T1, operand2: T2): Value
  def division(operand1: T1, operand2: T2): Value

trait Relational[T1 <: Value, T2 <: Value]:
  def equal(operand1: T1, operand2: T2): Boolean
  def lessThan(operand1: T1, operand2: T2): Boolean
  def greaterThan(operand1: T1, operand2: T2): Boolean

trait Reader:
  def read(input: String): Either[String, Value] =
    Left(s"'$input' is not understood")

type ArithmeticDispatcher = Dispatcher[Arithmetic]
type RelationalDispatcher = Dispatcher[Relational]

private def unrecognizedName(name: String) : String = s"$name is not recognized"

given ExpressionEvaluator[ExpressionNode] with
  extension (t: ExpressionNode) override def evaluateExpression(using environment: Environment)
                                                               (using functionDefinitionTable: FunctionDefinitionTable)
                                                               (using reader: Reader): Either[String, Value]=
    t match
      case n:IntegerValueExpressionNode =>
        summon[ExpressionEvaluator[IntegerValueExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: VariableExpressionNode =>
        summon[ExpressionEvaluator[VariableExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: IfExpressionNode =>
        summon[ExpressionEvaluator[IfExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: SetExpressionNode =>
        summon[ExpressionEvaluator[SetExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: WhileExpressionNode =>
        summon[ExpressionEvaluator[WhileExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: BeginExpressionNode =>
        summon[ExpressionEvaluator[BeginExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: FunctionCallExpressionNode =>
        summon[ExpressionEvaluator[FunctionCallExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: AdditionExpressionNode =>
        summon[ExpressionEvaluator[AdditionExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: SubtractionExpressionNode =>
        summon[ExpressionEvaluator[SubtractionExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: MultiplicationExpressionNode =>
        summon[ExpressionEvaluator[MultiplicationExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: DivisionExpressionNode =>
        summon[ExpressionEvaluator[DivisionExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: EqualityExpressionNode =>
        summon[ExpressionEvaluator[EqualityExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: LessThanExpressionNode =>
        summon[ExpressionEvaluator[LessThanExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: GreaterThanExpressionNode =>
        summon[ExpressionEvaluator[GreaterThanExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: PrintExpressionNode =>
        summon[ExpressionEvaluator[PrintExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case n: ReadExpressionNode =>
        summon[ExpressionEvaluator[ReadExpressionNode]].evaluateExpression(n)(using environment)(using functionDefinitionTable)
      case null =>
        Left("Not implemented")


given ExpressionEvaluator[IntegerValueExpressionNode] with
  extension (t: IntegerValueExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                           (using functionDefinitionTable: FunctionDefinitionTable)
                                                                           (using reader: Reader): Either[String, Value] =
    Right(IntegerValue(t.integerValue))

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

def add(v1: Value, v2: Value)(using dispatcher: ArithmeticDispatcher): Either[String, Value] =
  dispatcher.dispatch(v1, v2) match
    case Some(arithmetic) => Right(arithmetic.addition(v1, v2))
    case None =>
      Left(s"Addition is not supported for ${v1.getClass} and ${v2.getClass}")

given ExpressionEvaluator[AdditionExpressionNode] with
  extension (t: AdditionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => add(params.head, params(1))

def sub(v1: Value, v2: Value)(using dispatcher: ArithmeticDispatcher): Either[String, Value] =
  dispatcher.dispatch(v1, v2) match
    case Some(arithmetic) => Right(arithmetic.subtraction(v1, v2))
    case None =>
      Left(s"Subtraction is not supported for ${v1.getClass} and ${v2.getClass}")

given ExpressionEvaluator[SubtractionExpressionNode] with
  extension (t: SubtractionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => sub(params.head, params(1))

def mul(v1: Value, v2: Value)(using dispatcher: ArithmeticDispatcher): Either[String, Value] =
  dispatcher.dispatch(v1, v2) match
    case Some(arithmetic) => Right(arithmetic.multiplication(v1, v2))
    case None =>
      Left(s"Multiplication is not supported for ${v1.getClass} and ${v2.getClass}")

given ExpressionEvaluator[MultiplicationExpressionNode] with
  extension (t: MultiplicationExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                             (using functionDefinitionTable: FunctionDefinitionTable)
                                                                             (using reader: Reader): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => mul(params.head, params(1))

def div(v1: Value, v2: Value)(using dispatcher: ArithmeticDispatcher): Either[String, Value] =
  dispatcher.dispatch(v1, v2) match
    case Some(arithmetic) => Right(arithmetic.division(v1, v2))
    case None =>
      Left(s"Division is not supported for ${v1.getClass} and ${v2.getClass}")

given ExpressionEvaluator[DivisionExpressionNode] with
  extension (t: DivisionExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader): Either[String, Value] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) => div(params.head, params(1))

def compare(v1: Value, v2: Value)(using dispatcher: RelationalDispatcher): Either[String, Boolean] =
  dispatcher.dispatch(v1, v2) match
    case Some(relational) => Right(relational.equal(v1, v2))
    case None =>
      Left(s"Equal is not supported for ${v1.getClass} and ${v2.getClass}")

given ExpressionEvaluator[EqualityExpressionNode] with
  extension (t: EqualityExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) =>
        compare(params.head, params(1)) match
          case Left(e) => Left(e)
          case Right(v: Boolean) => Right(if v then IntegerValue.True else IntegerValue.False)

def lessThan(v1: Value, v2: Value)(using dispatcher: RelationalDispatcher): Either[String, Boolean] =
  dispatcher.dispatch(v1, v2) match
    case Some(relational) => Right(relational.lessThan(v1, v2))
    case None =>
      Left(s"Less than is not supported for ${v1.getClass} and ${v2.getClass}")

given ExpressionEvaluator[LessThanExpressionNode] with
  extension (t: LessThanExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                       (using functionDefinitionTable: FunctionDefinitionTable)
                                                                       (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) =>
        lessThan(params.head, params(1)) match
          case Left(e) => Left(e)
          case Right(v: Boolean) => Right(if v then IntegerValue.True else IntegerValue.False)

def greaterThan(v1: Value, v2: Value)(using dispatcher: RelationalDispatcher): Either[String, Boolean] =
  dispatcher.dispatch(v1, v2) match
    case Some(relational) => Right(relational.greaterThan(v1, v2))
    case None =>
      Left(s"Less than is not supported for ${v1.getClass} and ${v2.getClass}")

given ExpressionEvaluator[GreaterThanExpressionNode] with
  extension (t: GreaterThanExpressionNode) override def evaluateExpression(using environment: Environment)
                                                                          (using functionDefinitionTable: FunctionDefinitionTable)
                                                                          (using reader: Reader): Either[String, Value] =
    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable, reader) match
      case Left(error) => Left(error)
      case Right(params) =>
        greaterThan(params.head, params(1)) match
          case Left(e) => Left(e)
          case Right(v: Boolean) => Right(if v then IntegerValue.True else IntegerValue.False)

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

given Arithmetic[IntegerValue, IntegerValue] with
  override def addition(operand1: IntegerValue, operand2: IntegerValue): Value =
    IntegerValue(operand1.value + operand2.value)

  override def subtraction(operand1: IntegerValue, operand2: IntegerValue): Value =
    IntegerValue(operand1.value - operand2.value)

  override def multiplication(operand1: IntegerValue, operand2: IntegerValue): Value =
    IntegerValue(operand1.value * operand2.value)

  override def division(operand1: IntegerValue, operand2: IntegerValue): Value =
    IntegerValue(operand1.value / operand2.value)

given Relational[IntegerValue, IntegerValue] with
  override def equal(operand1: IntegerValue, operand2: IntegerValue): Boolean =
    operand1.value == operand2.value

  override def greaterThan(operand1: IntegerValue, operand2: IntegerValue): Boolean =
    operand1.value > operand2.value

  override def lessThan(operand1: IntegerValue, operand2: IntegerValue): Boolean =
    operand1.value < operand2.value

given ArithmeticDispatcher =
  Dispatcher.create[IntegerValue, IntegerValue, Arithmetic]

given RelationalDispatcher =
  Dispatcher.create[IntegerValue, IntegerValue, Relational]

