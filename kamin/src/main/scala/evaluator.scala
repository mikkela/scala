import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Try, Success, Failure}

trait Evaluator[T <: ExpressionNode]:
  extension (t: T) def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int]

private def unrecognizedName(name: String) : String = s"$name is not recognized"

given Evaluator[ExpressionNode] with
  extension (t: ExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int]=
    t match
      case n:IntegerExpressionNode =>
        summon[Evaluator[IntegerExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: VariableExpressionNode =>
        summon[Evaluator[VariableExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: IfExpressionNode =>
        summon[Evaluator[IfExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: SetExpressionNode =>
        summon[Evaluator[SetExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: WhileExpressionNode =>
        summon[Evaluator[WhileExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: BeginExpressionNode =>
        summon[Evaluator[BeginExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: FunctionCallExpressionNode =>
        summon[Evaluator[FunctionCallExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: AdditionExpressionNode =>
        summon[Evaluator[AdditionExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: SubtractionExpressionNode =>
        summon[Evaluator[SubtractionExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: MultiplicationExpressionNode =>
        summon[Evaluator[MultiplicationExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: DivisionExpressionNode =>
        summon[Evaluator[DivisionExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: EqualityExpressionNode =>
        summon[Evaluator[EqualityExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: LessThanExpressionNode =>
        summon[Evaluator[LessThanExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: GreaterThanExpressionNode =>
        summon[Evaluator[GreaterThanExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: PrintExpressionNode =>
        summon[Evaluator[PrintExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case n: ReadExpressionNode =>
        summon[Evaluator[ReadExpressionNode]].evaluate(n)(using environment)(using functionDefinitionTable)
      case null =>
        Left("Not implemented")

given Evaluator[IntegerExpressionNode] with
  extension (t: IntegerExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =
    Right(t.integerValue)

given Evaluator[VariableExpressionNode] with
  extension (t: VariableExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =
    environment.get(t.variableExpression) match
      case Some(value) => Right(value)
      case None => Left(unrecognizedName(t.variableExpression))

given Evaluator[IfExpressionNode] with
  extension (t: IfExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =
    t.testExpression.evaluate.flatMap {
      case 0 => t.alternativeExpression.evaluate
      case _ => t.consequenceExpression.evaluate
    }

given Evaluator[SetExpressionNode] with
  extension (t: SetExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =
    t.value.evaluate match
      case Left(value) => Left(value)
      case Right(value) =>
        environment.set(t.variable, value)
        Right(value)

given Evaluator[WhileExpressionNode] with
  extension (t: WhileExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =
    @tailrec
    def evaluateLoop(): Either[String, Int] =
      t.testExpression.evaluate match
        case Left(error) => Left(error)
        case Right(test) if test == 0 => Right(0)
        case Right(_) =>
          t.bodyExpression.evaluate match
            case Left(error) => Left(error)
            case Right(_) => evaluateLoop() // Recur to continue the loop

    evaluateLoop()

given Evaluator[BeginExpressionNode] with
  extension (t: BeginExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =
    t.expressions.foldLeft[Either[String, Int]](Right(0)) { (acc, expr) =>
      acc match
        case Left(error) => Left(error)
        case Right(_) => expr.evaluate
    }

private def undefinedFunctionName(name: String): Left[String, Nothing] =
  Left(s"$name is not recognized as a function")

private def invalidFunctionArity(function: String, expectedArity: Int): Left[String, Nothing] =
  Left(s"$function requires $expectedArity arguments")

private def evaluateParameters(parameters: Seq[ExpressionNode],
                               environment: Environment,
                               functionDefinitionTable: FunctionDefinitionTable): Either[String, List[Int]] =
  parameters.foldLeft(Right(List.empty[Int]): Either[String, List[Int]]) { (acc, p) =>
    acc match
      case Left(error) => Left(error) // If there's already an error, keep it
      case Right(params) =>
        p.evaluate(using environment)(using functionDefinitionTable) match
          case Left(error) => Left(error) // Stop and return the error if evaluation fails
          case Right(result) => Right(params :+ result) // Append result to the list if successful
  }
given Evaluator[FunctionCallExpressionNode] with
  extension (t: FunctionCallExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =

    val parameters = evaluateParameters(t.expressions, environment, functionDefinitionTable)

    functionDefinitionTable.lookupFunctionDefinition(t.function) match
      case None => undefinedFunctionName(t.function)
      case Some(functionDefinition) =>
        parameters match
          case Right(params) if params.length == functionDefinition.arguments.length =>
            environment.openScope(functionDefinition.arguments)
            functionDefinition.arguments.zip(params).foreach((a, p) => environment.set(a, p))

            functionDefinition.expression.evaluate(using environment)(using functionDefinitionTable) match
              case Left(error) => Left(error)
              case Right(result) =>
                environment.closeScope()
                Right(result)
          case Right(params) => invalidFunctionArity(functionDefinition.function, functionDefinition.arguments.length)
          case Left(error) => Left(error)

given Evaluator[AdditionExpressionNode] with
  extension (t: AdditionExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable) match
      case Left(error) => Left(error)
      case Right(params) => Right(params.head + params(1))

given Evaluator[SubtractionExpressionNode] with
  extension (t: SubtractionExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable) match
      case Left(error) => Left(error)
      case Right(params) => Right(params.head - params(1))

given Evaluator[MultiplicationExpressionNode] with
  extension (t: MultiplicationExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable) match
      case Left(error) => Left(error)
      case Right(params) => Right(params.head * params(1))

given Evaluator[DivisionExpressionNode] with
  extension (t: DivisionExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable) match
      case Left(error) => Left(error)
      case Right(params) => Right(params.head / params(1))

given Evaluator[EqualityExpressionNode] with
  extension (t: EqualityExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable) match
      case Left(error) => Left(error)
      case Right(params) => Right(if params.head == params(1) then 1 else 0)

given Evaluator[LessThanExpressionNode] with
  extension (t: LessThanExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable) match
      case Left(error) => Left(error)
      case Right(params) => Right(if params.head < params(1) then 1 else 0)

given Evaluator[GreaterThanExpressionNode] with
  extension (t: GreaterThanExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =

    evaluateParameters(Seq(t.operand1, t.operand2), environment, functionDefinitionTable) match
      case Left(error) => Left(error)
      case Right(params) => Right(if params.head > params(1) then 1 else 0)

given Evaluator[PrintExpressionNode] with
  extension (t: PrintExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =

    evaluateParameters(Seq(t.argument), environment, functionDefinitionTable) match
      case Left(error) => Left(error)
      case Right(params) =>
        println(params.head)
        Right(params.head)

given Evaluator[ReadExpressionNode] with
  extension (t: ReadExpressionNode) override def evaluate(using environment: Environment)(using functionDefinitionTable: FunctionDefinitionTable): Either[String, Int] =
    val input = StdIn.readLine()

    Try(input.toInt) match {
      case Success(number) => Right(number)
      case Failure(_) => Left(s"$input not a valid integer!")
    }
