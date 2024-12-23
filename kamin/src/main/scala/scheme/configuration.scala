package kamin.scheme

import kamin.basic.BasicReader
import kamin.{AdditionExpressionNodeParser, AsteriskToken, BasicLanguageFamilyParserContext, BeginExpressionNodeParser, BeginToken, BooleanDefinition, DivisionExpressionNodeParser, EqualToken, EqualityExpressionNodeParser, Evaluator, ExpressionEvaluator, ExpressionNode, FunctionDefinitionTable, GlobalEnvironment, GreaterThanExpressionNodeParser, GreaterThanToken, IfExpressionNodeParser, IfToken, IntegerValueExpressionNodeParser, LeftParenthesisToken, LessThanExpressionNodeParser, LessThanToken, Lexer, MinusToken, MultiplicationExpressionNodeParser, Parser, PeekingIterator, PlusToken, PrintToken, QuoteToken, Reader, RightParenthesisToken, SetExpressionNodeParser, SetToken, SlashToken, SubtractionExpressionNodeParser, Token, Value, VariableExpressionNodeParser, WhileExpressionNodeParser, WhileToken, given_ExpressionEvaluator_ExpressionNode}
import kamin.lisp.{CarExpressionNodeParser, CarToken, CdrExpressionNodeParser, CdrToken, ConsExpressionNodeParser, ConsToken, ListTestExpressionNodeParser, ListTestToken, ListValue, NullTestExpressionNodeParser, NullTestToken, NumberTestExpressionNodeParser, NumberTestToken, SExpressionNode, SExpressionNodeParser, SymbolTestExpressionNodeParser, SymbolTestToken, SymbolValue}
import kamin.lisp.given_ExpressionEvaluator_SExpressionNode

object RegistriesSetup:
  def initialize(): Unit =
    kamin.RegistriesSetup.initialize()

    kamin.ExpressionEvaluatorRegistry.register(classOf[ValueOpExpressionNode], summon[ExpressionEvaluator[ValueOpExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[LambdaExpressionNode], summon[ExpressionEvaluator[LambdaExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[ExpressionListExpressionNode], summon[ExpressionEvaluator[ExpressionListExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[SExpressionNode], summon[ExpressionEvaluator[SExpressionNode]])

object SchemeLexer extends Lexer(
  Seq(LeftParenthesisToken, RightParenthesisToken, QuoteToken),
  Seq(EqualToken, LessThanToken, GreaterThanToken, PlusToken, MinusToken, AsteriskToken, SlashToken, PrintToken,
    IfToken, WhileToken, SetToken, BeginToken, ConsToken, CarToken, CdrToken, NumberTestToken,
    SymbolTestToken, ListTestToken, NullTestToken, PrimopTestToken, ClosureTestToken, LambdaToken
  )
)

object SchemeParser
  extends ExpressionListExpressionNodeParser
    with IntegerValueExpressionNodeParser
    with SExpressionNodeParser
    with VariableExpressionNodeParser
    with IfExpressionNodeParser
    with WhileExpressionNodeParser
    with SetExpressionNodeParser
    with BeginExpressionNodeParser
    with ValueOperationExpressionNodeParser
    with LambdaExpressionNodeParser

object SchemeParserContext extends BasicLanguageFamilyParserContext:
  override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
    SchemeParser.parse(tokens)(using this)

class SchemeEvaluator() extends Evaluator:
  override def evaluate(input: String): String =
    SchemeParser.parse(PeekingIterator[Token](SchemeLexer.tokens(input)))(using SchemeParserContext) match
      case Left(e: String) => e
      case Right(e: ExpressionNode) =>
        e.evaluateExpression(using GlobalEnvironment)(using FunctionDefinitionTable())(using BasicReader)(using new BooleanDefinition {
          override def trueValue: Value = SymbolValue.T
          override def falseValue: Value = ListValue.nil
        }) match
          case Left(l) => l
          case Right(r) => r.toString
