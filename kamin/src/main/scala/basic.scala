object BasicLexer extends Lexer(
  Seq(LeftParenthesisToken, RightParenthesisToken),
  Seq(EqualToken, LessThanToken, GreaterThanToken, PlusToken, MinusToken, AsteriskToken, SlashToken, PrintToken,
    DefineToken, IfToken, WhileToken, SetToken, BeginToken)
)

object BasicFunDefNodeParser extends FunctionDefinitionNodeParser

object BasicExpressionNodeParser extends IntegerValueExpressionNodeParser
  with VariableExpressionNodeParser
  with IfExpressionNodeParser
  with WhileExpressionNodeParser
  with SetExpressionNodeParser
  with BeginExpressionNodeParser
  with AdditionExpressionNodeParser
  with SubtractionExpressionNodeParser
  with MultiplicationExpressionNodeParser
  with DivisionExpressionNodeParser
  with EqualityExpressionNodeParser
  with LessThanExpressionNodeParser
  with GreaterThanExpressionNodeParser
  with PrintExpressionNodeParser
  with FunctionCallExpressionNodeParser

object BasicParser extends Parser[FunctionDefinitionNode | ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, FunctionDefinitionNode | ExpressionNode] =
    BasicFunDefNodeParser.parse(tokens) match
      case Right(value) => Right(value)
      case Left(_) => BasicExpressionNodeParser.parse(tokens)

object BasicParserContext extends BasicLanguageFamilyParserContext:
  override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
    BasicExpressionNodeParser.parse(tokens)(using this)

