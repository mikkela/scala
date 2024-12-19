package kamin.lisp

import kamin.{AdditionExpressionNodeParser, AsteriskToken, BasicLanguageFamilyParserContext, BeginExpressionNodeParser, BeginToken, BooleanDefinition, DefineToken, DivisionExpressionNodeParser, Environment, EqualToken, EqualityExpressionNodeParser, Evaluator, ExpressionEvaluator, ExpressionNode, FunctionCallExpressionNodeParser, FunctionDefinitionNode, FunctionDefinitionNodeParser, FunctionDefinitionTable, GlobalEnvironment, GreaterThanExpressionNodeParser, GreaterThanToken, IfExpressionNodeParser, IfToken, IntegerValue, IntegerValueExpressionNodeParser, IntegerValueReader, LeftParenthesisToken, LessThanExpressionNodeParser, LessThanToken, Lexer, MinusToken, MultiplicationExpressionNodeParser, Parser, PeekingIterator, PlusToken, PrintToken, QuoteToken, ReadToken, Reader, RightParenthesisToken, SetExpressionNodeParser, SetToken, SlashToken, SubtractionExpressionNodeParser, Token, Value, VariableExpressionNodeParser, WhileExpressionNodeParser, WhileToken, given_ExpressionEvaluator_ExpressionNode}

object RegistriesSetup:
  def initialize(): Unit =
    kamin.RegistriesSetup.initialize()

    kamin.ExpressionEvaluatorRegistry.register(classOf[SExpressionNode], summon[ExpressionEvaluator[SExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[ConsExpressionNode], summon[ExpressionEvaluator[ConsExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[CarExpressionNode], summon[ExpressionEvaluator[CarExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[CdrExpressionNode], summon[ExpressionEvaluator[CdrExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[NumberTestExpressionNode], summon[ExpressionEvaluator[NumberTestExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[SymbolTestExpressionNode], summon[ExpressionEvaluator[SymbolTestExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[ListTestExpressionNode], summon[ExpressionEvaluator[ListTestExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[NullTestExpressionNode], summon[ExpressionEvaluator[NullTestExpressionNode]])


object LispLexer extends Lexer(
  Seq(LeftParenthesisToken, RightParenthesisToken, QuoteToken),
  Seq(EqualToken, LessThanToken, GreaterThanToken, PlusToken, MinusToken, AsteriskToken, SlashToken, PrintToken,
    ReadToken, DefineToken, IfToken, WhileToken, SetToken, BeginToken, ConsToken, CarToken, CdrToken, NumberTestToken,
    SymbolTestToken, ListTestToken, NullTestToken)
)

object LispFunDefNodeParser extends FunctionDefinitionNodeParser

object LispExpressionNodeParser
  extends IntegerValueExpressionNodeParser
    with SExpressionNodeParser
    with VariableExpressionNodeParser
    with IfExpressionNodeParser
    with WhileExpressionNodeParser
    with SetExpressionNodeParser
    with BeginExpressionNodeParser
    with FunctionCallExpressionNodeParser
    with AdditionExpressionNodeParser
    with SubtractionExpressionNodeParser
    with MultiplicationExpressionNodeParser
    with DivisionExpressionNodeParser
    with EqualityExpressionNodeParser
    with GreaterThanExpressionNodeParser
    with LessThanExpressionNodeParser
    with ConsExpressionNodeParser
    with CarExpressionNodeParser
    with CdrExpressionNodeParser
    with NumberTestExpressionNodeParser
    with SymbolTestExpressionNodeParser
    with ListTestExpressionNodeParser
    with NullTestExpressionNodeParser

object LispParser extends Parser[FunctionDefinitionNode | ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, FunctionDefinitionNode | ExpressionNode] =
    LispFunDefNodeParser.parse(tokens) match
      case Right(value) => Right(value)
      case Left(_) => LispExpressionNodeParser.parse(tokens)

object LispParserContext extends BasicLanguageFamilyParserContext:
  override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
    LispExpressionNodeParser.parse(tokens)(using this)

object LispReader extends IntegerValueReader

class LispEvaluator() extends Evaluator:
  val functionDefinitionTable: FunctionDefinitionTable = FunctionDefinitionTable()
  val reader: Reader = LispReader
  override def evaluate(input: String): String =
    LispParser.parse(PeekingIterator[Token](LispLexer.tokens(input)))(using LispParserContext) match
      case Left(e: String) => e
      case Right(f: FunctionDefinitionNode) =>
        functionDefinitionTable.register(f)
        f.function
      case Right(e: ExpressionNode) =>
        e.evaluateExpression(using GlobalEnvironment)(using functionDefinitionTable)(using reader)(using new BooleanDefinition {
          override def trueValue: Value = SymbolValue.T
          override def falseValue: Value = ListValue.nil
        }) match
          case Left(l) => l
          case Right(r) => r.toString
