package kamin.basic

import kamin.{AdditionExpressionNodeParser, AsteriskToken, BasicLanguageFamilyParserContext, BeginExpressionNodeParser, BeginToken, DefineToken, DivisionExpressionNodeParser, Environment, EqualToken, EqualityExpressionNodeParser, Evaluator, ExpressionNode, FunctionCallExpressionNodeParser, FunctionDefinitionNode, FunctionDefinitionNodeParser, FunctionDefinitionTable, GlobalAndLocalScopeEnvironment, GreaterThanExpressionNodeParser, GreaterThanToken, IfExpressionNodeParser, IfToken, IntegerValue, IntegerValueExpressionNodeParser, IntegerValueReader, LeftParenthesisToken, LessThanExpressionNodeParser, LessThanToken, Lexer, MinusToken, MultiplicationExpressionNodeParser, Parser, PeekingIterator, PlusToken, PrintExpressionNodeParser, PrintToken, ReadExpressionNodeParser, ReadToken, Reader, RightParenthesisToken, SetExpressionNodeParser, SetToken, SlashToken, SubtractionExpressionNodeParser, Token, VariableExpressionNodeParser, WhileExpressionNodeParser, WhileToken, given_ExpressionEvaluator_ExpressionNode}

object BasicLexer extends Lexer(
  Seq(LeftParenthesisToken, RightParenthesisToken),
  Seq(EqualToken, LessThanToken, GreaterThanToken, PlusToken, MinusToken, AsteriskToken, SlashToken, PrintToken,
    ReadToken, DefineToken, IfToken, WhileToken, SetToken, BeginToken)
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
  with ReadExpressionNodeParser
  with FunctionCallExpressionNodeParser

object BasicParser extends Parser[FunctionDefinitionNode | ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, FunctionDefinitionNode | ExpressionNode] =
    BasicFunDefNodeParser.parse(tokens) match
      case Right(value) => Right(value)
      case Left(_) => BasicExpressionNodeParser.parse(tokens)

object BasicParserContext extends BasicLanguageFamilyParserContext:
  override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
    BasicExpressionNodeParser.parse(tokens)(using this)

object BasicReader extends IntegerValueReader

class BasicEvaluator() extends Evaluator:
  val functionDefinitionTable: FunctionDefinitionTable = FunctionDefinitionTable()
  val environment: Environment = GlobalAndLocalScopeEnvironment()
  val reader: Reader = BasicReader
  override def evaluate(input: String): String =
    BasicParser.parse(PeekingIterator[Token](BasicLexer.tokens(input)))(using BasicParserContext) match
      case Left(e: String) => e
      case Right(f: FunctionDefinitionNode) =>
        functionDefinitionTable.register(f)
        f.function
      case Right(e: ExpressionNode) =>
        e.evaluateExpression(using environment)(using functionDefinitionTable)(using reader)(using IntegerValue.False) match
          case Left(l) => l
          case Right(r) => r.toString


