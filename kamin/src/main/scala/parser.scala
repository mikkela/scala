import scala.util.Right

trait ParserContext

trait BasicLanguageFamilyParserContext extends ParserContext:
  def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode]

private def invalidToken(token: Token): Left[String, Nothing] =
  Left(s"${token.literal} is an unexpected token")

private def invalidEndOfProgram: Either[String, Nothing] =
  Left("Invalid end of program")

private def invalidArity(expectedOperator: String, expectedArity: Int): Left[String, Nothing] =
  Left(s"$expectedOperator requires $expectedArity arguments")

private def unexpectedError: Either[String, Nothing] =
  Left("Unexpected error")

private def checkTokensForPresence(tokens: PeekingIterator[Token], expected: (Token=>Boolean)*): Either[String, Seq[Token]] =
  val peeks = tokens.peek(expected.length)
  if peeks.length < expected.length then
    return invalidEndOfProgram
  expected.zip(peeks).foldLeft[Either[String, List[Token]]](Right(Nil)) {
    case (Right(acc), (expectedType, peek)) =>
      if expectedType(peek) then Right(acc :+ peek)
      else invalidToken(peek)
    case (left, _) => left // Early termination if error found
  }

private def parseListOfElements[ElementType](tokens: PeekingIterator[Token], elementParser: PeekingIterator[Token] => Either[String, ElementType]): Either[String, Seq[ElementType]] =
  var list = List.empty[ElementType]
  var peek = tokens.peek(1)
  while (peek.nonEmpty && !peek.head.isToken(RightParenthesisToken))
    elementParser(tokens) match
      case Left(value) => return Left(value)
      case Right(element) =>
        list = list :+ element
    peek = tokens.peek(1)
  Right(list)

private def parseFixedNumberOfElements[ElementType <: Node](tokens: PeekingIterator[Token], count: Int,
                                                                elementParser: PeekingIterator[Token] => Either[String, ElementType]):
  Either[String, Seq[ElementType]] =
    (1 to count).foldLeft(Right(Nil): Either[String, List[ElementType]]) { (acc, _) =>
      acc.flatMap { elements =>
        elementParser(tokens).map(elements :+ _)
      }
    }

implicit class TokenExtensions(val t: Token) extends AnyVal {
  def isToken(o: Token): Boolean = t == o
  def isNameToken: Boolean = t match
    case NameToken(_) => true
    case _ => false
  def isIntegerValueToken: Boolean = t match
    case IntegerValueToken(_) => true
    case _ => false
}

trait Parser[ResultType <: InputNode, ParserContextType <: ParserContext]:
  def parse(tokens: PeekingIterator[Token])(using context: ParserContextType): Either[String, ResultType] =
    val peeking = tokens.peek(1)
    if peeking.isEmpty then
      invalidEndOfProgram
    else
      invalidToken(peeking.head)



trait FunctionDefinitionNodeParser extends Parser[FunctionDefinitionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, FunctionDefinitionNode] =
    checkTokensForPresence(tokens, _.isToken(LeftParenthesisToken), _.isToken(DefineToken)) match
      case Left(_) => super.parse(tokens) // Handle fallback case directly
      case Right(_) =>
        tokens.consumeTokens(2) // Skip '(' and 'define'
        checkTokensForPresence(tokens, _.isNameToken) match
          case Right(Seq(NameToken(name))) =>
            tokens.consumeTokens(1)
            checkTokensForPresence(tokens, _.isToken(LeftParenthesisToken)) match
              case Right(_) =>
                tokens.consumeTokens(1)
                parseListOfElements(tokens, t =>
                  t.peek(1) match
                    case Seq(NameToken(literal)) =>
                      tokens.consumeTokens(1)
                      Right(literal)
                    case Seq(token) => invalidToken(token)
                    case _ => invalidEndOfProgram
                ) match
                  case Right(args) =>
                    checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
                      case Right(_) =>
                        tokens.consumeTokens(1)
                        parseFixedNumberOfElements(tokens, 1, context.parseExpression) match
                          case Right(Seq(expression)) =>
                            checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
                            case Right(_) =>
                              tokens.consumeTokens(1)
                              Right(FunctionDefinitionNode(name, args, expression))
                            case Left(value) => Left(value)
                          case Right(_) => unexpectedError
                          case Left(value) => Left(value)
                      case Left(value) => Left(value)
                  case Left(value) => Left(value)
              case Left(value) => Left(value)
          case Right(_) => unexpectedError
          case Left(value) => Left(value)

trait IntegerValueExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isIntegerValueToken) match
      case Right(Seq(value)) =>
        tokens.consumeTokens(1)
        Right(IntegerValueExpressionNode(value.literal.toInt))
      case _ => super.parse(tokens)


trait VariableExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isNameToken) match
      case Right(Seq(value)) =>
        tokens.consumeTokens(1)
        Right(VariableExpressionNode(value.literal))
      case _ => super.parse(tokens)

trait IfExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isToken(LeftParenthesisToken), _.isToken(IfToken)) match
      case Right(_) =>
        tokens.consumeTokens(2)
        parseFixedNumberOfElements(tokens, 3, context.parseExpression) match
          case Left(value) => Left(value)
          case Right(Seq(testExpression, consequenceExpression, alternativeExpression)) =>
            checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
              case Left(value) => Left(value)
              case Right(_) =>
                tokens.consumeTokens(1)
                Right(IfExpressionNode(testExpression, consequenceExpression, alternativeExpression))
          case Right(_) => unexpectedError
      case _ => super.parse(tokens)

trait WhileExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isToken(LeftParenthesisToken), _.isToken(WhileToken)) match
      case Right(_) =>
        tokens.consumeTokens(2)
        parseFixedNumberOfElements(tokens, 2, context.parseExpression) match
          case Left(value) => Left(value)
          case Right(Seq(testExpr, bodyExpr)) =>
            checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
              case Left(value) => Left(value)
              case Right(_) =>
                tokens.consumeTokens(1)
                Right(WhileExpressionNode(testExpr, bodyExpr))
          case Right(_) => unexpectedError
      case _ => super.parse(tokens)

trait SetExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isToken(LeftParenthesisToken), _.isToken(SetToken)) match
      case Right(_) =>
        tokens.consumeTokens(2)
        checkTokensForPresence(tokens, _.isNameToken) match
          case Right(Seq(NameToken(variable))) =>
            tokens.consumeTokens(1)
            parseFixedNumberOfElements(tokens, 1, context.parseExpression) match
              case Left(value) => Left(value)
              case Right(Seq(valueExpression)) =>
                checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
                  case Left(value) => Left(value)
                  case Right(_) =>
                    tokens.consumeTokens(1)
                    Right(SetExpressionNode(variable, valueExpression))
              case Right(_) => unexpectedError
          case Right(_) => unexpectedError
          case Left(value) => Left(value)
      case _ => super.parse(tokens)

trait BeginExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isToken(LeftParenthesisToken), _.isToken(BeginToken)) match
      case Right(_) =>
        tokens.consumeTokens(2)
        parseListOfElements(tokens, context.parseExpression) match
          case Left(value) => Left(value)
          case Right(expressions) if expressions.nonEmpty =>
            checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
              case Left(value) => Left(value)
              case Right(_) =>
                tokens.consumeTokens(1)
                Right(BeginExpressionNode(expressions))
          case Right(_) =>
            tokens.peek(1) match
              case List(token) => invalidToken(token)
              case _ => invalidEndOfProgram
      case _ => super.parse(tokens)

def parseOperator(tokens: PeekingIterator[Token],
                  isExpectedOperator: Token => Boolean,
                  expectedArity: Option[Int],
                  producer: (String, Seq[ExpressionNode]) => ExpressionNode,
                  context: BasicLanguageFamilyParserContext,
                  continueChain: PeekingIterator[Token] => Either[String, ExpressionNode]): Either[String, ExpressionNode] =
  checkTokensForPresence(tokens, _.isToken(LeftParenthesisToken), isExpectedOperator) match
    case Left(_) => continueChain(tokens)
    case Right(Seq(_, operator: Token)) =>
      tokens.consumeTokens(2)
      parseListOfElements(tokens, t => context.parseExpression(t)) match
        case Left(value) => Left(value)
        case Right(expressions) if expressions.length == expectedArity.getOrElse(expressions.length) =>
          checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
            case Left(value) => Left(value)
            case Right(_) =>
              tokens.consumeTokens(1)
              Right(producer(operator.literal, expressions))
        case Right(expressions) => invalidArity(operator.literal, expectedArity.get)
    case Right(_) => unexpectedError

trait AdditionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(PlusToken), Some(2), (_, expressions) => AdditionExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait SubtractionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(MinusToken), Some(2), (_, expressions) => SubtractionExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait MultiplicationExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(AsteriskToken), Some(2), (_, expressions) => MultiplicationExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait DivisionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(SlashToken), Some(2), (_, expressions) => DivisionExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait EqualityExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(EqualToken), Some(2), (_, expressions) => EqualityExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait LessThanExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(LessThanToken), Some(2), (_, expressions) => LessThanExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait GreaterThanExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(GreaterThanToken), Some(2), (_, expressions) => GreaterThanExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait PrintExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(PrintToken), Some(1), (_, expressions) => PrintExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait ReadExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(ReadToken), Some(0), (_, expressions) => ReadExpressionNode(),
      context, tokens => super.parse(tokens)(using context))

trait FunctionCallExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isNameToken, None, (name, expressions) => FunctionCallExpressionNode(name, expressions),
      context, tokens => super.parse(tokens)(using context))
