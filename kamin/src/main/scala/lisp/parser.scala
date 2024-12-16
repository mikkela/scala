package kamin.lisp

import kamin.{BasicLanguageFamilyParserContext, ExpressionNode, IntegerValue, IntegerValueExpressionNode, IntegerValueToken, LeftParenthesisToken, NameToken, Parser, PeekingIterator, QuoteToken, RightParenthesisToken, Token, TokenExtensions, checkTokensForPresence, invalidEndOfProgram, invalidToken, parseListOfElements, parseOperator}

def isSymbolPart(token: Token) = token.isToken(LeftParenthesisToken) || !token.isToken(RightParenthesisToken)
trait SExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isToken(QuoteToken), t => isSymbolPart(t)) match
      case Right(_) =>
        tokens.consumeTokens(1)
        parseSExpression(tokens, context)
      case _ => super.parse(tokens)
  def parseSExpression(tokens: PeekingIterator[Token], context: BasicLanguageFamilyParserContext): Either[String, SExpressionNode] =
    checkTokensForPresence(tokens, t => isSymbolPart(t)) match
      case Right(Seq(integer: IntegerValueToken)) =>
        context.parseExpression(tokens) match
          case Right(value: IntegerValueExpressionNode) => Right(SExpressionNode(value))
          case Right(_) => Left("Integer did not parse to integer value expression in parsing S-expression")
          case Left(error) => Left(error)
      case Right(Seq(symbol)) if symbol != LeftParenthesisToken =>
        tokens.consumeTokens(1)
        Right(SExpressionNode(SymbolExpressionNode(symbol.literal)))
      case Right(Seq(LeftParenthesisToken)) =>
        tokens.consumeTokens(1)
        parseListOfElements(
          tokens, tokens => this.parseSExpression(tokens, context)) match
          case Left(value) => Left(value)
          case Right(values: Seq[SExpressionNode]) =>
            checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
              case Left(value) => Left(value)
              case Right(_) =>
                tokens.consumeTokens(1)
                Right(SExpressionNode(values))
          case Right(_) =>
            tokens.peek(1) match
              case List(token) => invalidToken(token)
              case _ => invalidEndOfProgram


trait ConsExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(ConsToken), Some(2), (_, expressions) => ConsExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait CarExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(CarToken), Some(1), (_, expressions) => CarExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait CdrExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(CdrToken), Some(1), (_, expressions) => CdrExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait NumberTestExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(NumberTestToken), Some(1), (_, expressions) => NumberTestExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait SymbolTestExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(SymbolTestToken), Some(1), (_, expressions) => SymbolTestExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait ListTestExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(ListTestToken), Some(1), (_, expressions) => ListTestExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait NullTestExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, _.isToken(NullTestToken), Some(1), (_, expressions) => NullTestExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))
