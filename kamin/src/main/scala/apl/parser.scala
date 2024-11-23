package kamin.apl

import kamin.Parser
import kamin.ExpressionNode
import kamin.BasicLanguageFamilyParserContext
import kamin.PeekingIterator
import kamin.parseOperator
import kamin.Token
import kamin.apl.{MaxToken, AndToken, OrToken, PlusSlashToken, MinusSlashToken, AsteriskSlashToken, SlashSlashToken, MaxSlashToken, OrSlashToken, AndSlashToken}
import kamin.TokenExtensions

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
