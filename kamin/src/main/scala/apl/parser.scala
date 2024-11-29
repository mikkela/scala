package kamin.apl

import kamin.{BasicLanguageFamilyParserContext, ExpressionNode, IntegerValueToken, LeftParenthesisToken, Parser, PeekingIterator, QuoteToken, RightParenthesisToken, Token, TokenExtensions, checkTokensForPresence, invalidEndOfProgram, invalidToken, parseInteger, parseListOfElements, parseOperator}
import kamin.apl.{AndSlashToken, AndToken, AsteriskSlashToken, MaxSlashToken, MaxToken, MinusSlashToken, OrSlashToken, OrToken, PlusSlashToken, SlashSlashToken}

trait VectorValueExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isToken(QuoteToken), _.isToken(LeftParenthesisToken)) match
      case Right(_) =>
        tokens.consumeTokens(2)
        parseListOfElements(
          tokens, parseInteger) match
          case Left(value) => Left(value)
          case Right(integers)  =>
            checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
              case Left(value) => Left(value)
              case Right(_) =>
                tokens.consumeTokens(1)
                Right(VectorValueExpressionNode(VectorValue.createVector(integers)))
          case Right(_) =>
            tokens.peek(1) match
              case List(token) => invalidToken(token)
              case _ => invalidEndOfProgram
      case _ => super.parse(tokens)
          
      
trait MaximumExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(MaxToken), Some(2), (_, expressions) => MaximumExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait AndExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(AndToken), Some(2), (_, expressions) => AndExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait OrExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(OrToken), Some(2), (_, expressions) => OrExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait AdditionReductionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(PlusSlashToken), Some(1), (_, expressions) => AdditionReductionExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait SubtractionReductionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(MinusSlashToken), Some(1), (_, expressions) => SubtractionReductionExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait MultiplicationReductionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(AsteriskSlashToken), Some(1), (_, expressions) => MultiplicationReductionExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait DivisionReductionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(SlashSlashToken), Some(1), (_, expressions) => DivisionReductionExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait MaximumReductionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(MaxSlashToken), Some(1), (_, expressions) => MaximumReductionExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait AndReductionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(AndSlashToken), Some(1), (_, expressions) => AndReductionExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait OrReductionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(OrSlashToken), Some(1), (_, expressions) => OrReductionExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))


trait CompressionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(CompressToken), Some(2), (_, expressions) => CompressionExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait ShapeExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(ShapeToken), Some(1), (_, expressions) => ShapeExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait RavelingExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(RavelToken), Some(1), (_, expressions) => RavelingExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait RestructuringExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(RestructToken), Some(2), (_, expressions) => RestructuringExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait CatenationExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(CatToken), Some(2), (_, expressions) => CatenationExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait IndexGenerationExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(IndxToken), Some(1), (_, expressions) => IndexGenerationExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait TranspositionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(TransToken), Some(1), (_, expressions) => TranspositionExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait SubscriptingExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(SquareBracketsToken), Some(2), (_, expressions) => SubscriptingExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))
