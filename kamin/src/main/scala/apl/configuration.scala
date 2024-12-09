package kamin.apl

import kamin.{AdditionExpressionNodeParser, AsteriskToken, BasicLanguageFamilyParserContext, BeginExpressionNodeParser, BeginToken, DefineToken, DivisionExpressionNodeParser, Environment, EqualToken, EqualityExpressionNodeParser, Evaluator, ExpressionEvaluator, ExpressionNode, FunctionCallExpressionNodeParser, FunctionDefinitionNode, FunctionDefinitionNodeParser, FunctionDefinitionTable, GlobalAndLocalScopeEnvironment, GreaterThanExpressionNodeParser, GreaterThanToken, IfExpressionNodeParser, IfToken, IntegerValue, IntegerValueExpressionNodeParser, IntegerValueReader, LeftParenthesisToken, LessThanExpressionNodeParser, LessThanToken, Lexer, MinusToken, MultiplicationExpressionNodeParser, Parser, PeekingIterator, PlusToken, PrintToken, QuoteToken, ReadToken, Reader, RightParenthesisToken, SetExpressionNodeParser, SetToken, SlashToken, SubtractionExpressionNodeParser, Token, VariableExpressionNodeParser, WhileExpressionNodeParser, WhileToken, given_ExpressionEvaluator_ExpressionNode}

object RegistriesSetup:
  def initialize(): Unit =
    kamin.RegistriesSetup.initialize()

    kamin.ExpressionEvaluatorRegistry.register(classOf[VectorValueExpressionNode], summon[ExpressionEvaluator[VectorValueExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[MaximumExpressionNode], summon[ExpressionEvaluator[MaximumExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[AndExpressionNode], summon[ExpressionEvaluator[AndExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[OrExpressionNode], summon[ExpressionEvaluator[OrExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[AdditionReductionExpressionNode], summon[ExpressionEvaluator[AdditionReductionExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[SubtractionReductionExpressionNode], summon[ExpressionEvaluator[SubtractionReductionExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[MultiplicationReductionExpressionNode], summon[ExpressionEvaluator[MultiplicationReductionExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[DivisionReductionExpressionNode], summon[ExpressionEvaluator[DivisionReductionExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[MaximumReductionExpressionNode], summon[ExpressionEvaluator[MaximumReductionExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[AndReductionExpressionNode], summon[ExpressionEvaluator[AndReductionExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[OrReductionExpressionNode], summon[ExpressionEvaluator[OrReductionExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[CompressionExpressionNode], summon[ExpressionEvaluator[CompressionExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[ShapeExpressionNode], summon[ExpressionEvaluator[ShapeExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[RavelingExpressionNode], summon[ExpressionEvaluator[RavelingExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[RestructuringExpressionNode], summon[ExpressionEvaluator[RestructuringExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[CatenationExpressionNode], summon[ExpressionEvaluator[CatenationExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[IndexGenerationExpressionNode], summon[ExpressionEvaluator[IndexGenerationExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[TranspositionExpressionNode], summon[ExpressionEvaluator[TranspositionExpressionNode]])
    kamin.ExpressionEvaluatorRegistry.register(classOf[SubscriptingExpressionNode], summon[ExpressionEvaluator[SubscriptingExpressionNode]])

object APLLexer extends Lexer(
  Seq(LeftParenthesisToken, RightParenthesisToken, QuoteToken),
  Seq(EqualToken, LessThanToken, GreaterThanToken, PlusToken, MinusToken, AsteriskToken, SlashToken, PrintToken,
    ReadToken, DefineToken, IfToken, WhileToken, SetToken, BeginToken, MaxToken, AndToken, OrToken, PlusSlashToken,
    MinusSlashToken, AsteriskSlashToken, SlashSlashToken, MaxSlashToken, AndSlashToken, OrSlashToken,
    CompressToken, ShapeToken, RavelToken, RestructToken, CatToken, IndxToken, TransToken, SquareBracketsToken)
)

object APLFunDefNodeParser extends FunctionDefinitionNodeParser

object APLExpressionNodeParser
  extends IntegerValueExpressionNodeParser
  with VectorValueExpressionNodeParser
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
  with MaximumExpressionNodeParser
  with AndExpressionNodeParser
  with OrExpressionNodeParser
  with AndReductionExpressionNodeParser
  with OrReductionExpressionNodeParser
  with MaximumReductionExpressionNodeParser
  with AdditionReductionExpressionNodeParser
  with SubtractionReductionExpressionNodeParser
  with MultiplicationReductionExpressionNodeParser
  with DivisionReductionExpressionNodeParser
  with CompressionExpressionNodeParser
  with ShapeExpressionNodeParser
  with RavelingExpressionNodeParser
  with RestructuringExpressionNodeParser
  with CatenationExpressionNodeParser
  with IndexGenerationExpressionNodeParser
  with TranspositionExpressionNodeParser
  with SubscriptingExpressionNodeParser

object APLParser extends Parser[FunctionDefinitionNode | ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, FunctionDefinitionNode | ExpressionNode] =
    APLFunDefNodeParser.parse(tokens) match
      case Right(value) => Right(value)
      case Left(_) => APLExpressionNodeParser.parse(tokens)

object APLParserContext extends BasicLanguageFamilyParserContext:
  override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
    APLExpressionNodeParser.parse(tokens)(using this)

object APLReader extends IntegerValueReader
  with VectorValueReader

class APLEvaluator() extends Evaluator:
  val functionDefinitionTable: FunctionDefinitionTable = FunctionDefinitionTable()
  val environment: Environment = GlobalAndLocalScopeEnvironment()
  val reader: Reader = APLReader
  override def evaluate(input: String): String =
    APLParser.parse(PeekingIterator[Token](APLLexer.tokens(input)))(using APLParserContext) match
      case Left(e: String) => e
      case Right(f: FunctionDefinitionNode) =>
        functionDefinitionTable.register(f)
        f.function
      case Right(e: ExpressionNode) =>
        e.evaluateExpression(using environment)(using functionDefinitionTable)(using reader)(using IntegerValue.False) match
          case Left(l) => l
          case Right(r) => r.toString




