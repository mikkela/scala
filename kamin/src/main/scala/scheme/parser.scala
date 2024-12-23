package kamin.scheme

import kamin.{Evaluator, BasicLanguageFamilyParserContext, DefineToken, ExpressionNode, FunctionDefinitionNode, LeftParenthesisToken, LessThanToken, NameToken, Parser, PeekingIterator, RightParenthesisToken, SlashToken, Token, VariableExpressionNode}
import kamin.{checkTokensForPresence, parseListOfElements, parseFixedNumberOfElements, Value, unexpectedError}
import kamin.{LessThanToken, GreaterThanToken, EqualToken, AsteriskToken, SlashToken, PlusToken, MinusToken}
import kamin.{TokenExtensions, invalidToken, invalidEndOfProgram}
import kamin.given_ExpressionEvaluator_ExpressionNode
import kamin.lisp.{ListValue}
import kamin.lisp.{CarToken, CdrToken, ConsToken, ListTestToken, NullTestToken, NumberTestToken, SymbolTestToken}
def isValueOperator(token: Token) =
  token match
    case PlusToken => true
    case MinusToken => true
    case AsteriskToken => true
    case SlashToken => true
    case EqualToken => true
    case GreaterThanToken => true
    case LessThanToken => true
    case CarToken => true
    case CdrToken => true
    case ConsToken => true
    case NumberTestToken => true
    case SymbolTestToken => true
    case ListTestToken => true
    case NullTestToken => true
    case PrimopTestToken => true
    case ClosureTestToken => true
    case _ => false

trait ValueOperationExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, isValueOperator(_)) match
      case Right(Seq(value)) =>
        tokens.consumeTokens(1)
        Right(ValueOpExpressionNode(value.literal))
      case _ => super.parse(tokens)

trait LambdaExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isToken(LeftParenthesisToken), _.isToken(LambdaToken)) match
      case Left(_) => super.parse(tokens) // Handle fallback case directly
      case Right(_) =>
        tokens.consumeTokens(2) // Skip '(' and 'lambda'
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
                            Right(LambdaExpressionNode(args, expression))
                          case Left(value) => Left(value)
                      case Right(_) => unexpectedError
                      case Left(value) => Left(value)
                  case Left(value) => Left(value)
              case Left(value) => Left(value)
          case Left(value) => Left(value)

trait ExpressionListExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, _.isToken(LeftParenthesisToken)) match
    case Left(_) => super.parse(tokens)
    case Right(_) =>
      tokens.consumeTokens(1)
      parseListOfElements(tokens, t => context.parseExpression(t)) match
        case Left(value) => Left(value)
        case Right(expressions) =>
          checkTokensForPresence(tokens, _.isToken(RightParenthesisToken)) match
            case Left(value) => Left(value)
            case Right(_) =>
              tokens.consumeTokens(1)
              Right(ExpressionListExpressionNode(expressions))
    case Right(_) => unexpectedError