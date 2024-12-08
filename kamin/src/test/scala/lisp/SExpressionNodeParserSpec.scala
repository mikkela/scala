package kamin.lisp

import kamin.{IntegerValue, BasicLanguageFamilyParserContext, ExpressionNode, IntegerValueExpressionNode, IntegerValueToken, LeftParenthesisToken, NameToken, PeekingIterator, QuoteToken, RightParenthesisToken, Token}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SExpressionNodeParserSpec extends AnyFunSpec with Matchers {

  describe("SExpressionNodeParser") {

    it("should parse an integer token to an SExpressionNode with an IntegerValueExpressionNode") {
      val tokens = PeekingIterator(Seq(QuoteToken, IntegerValueToken("42")).iterator)
      val parser = new SExpressionNodeParser {}
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          tokens.consumeTokens(1)
          Right(IntegerValueExpressionNode(42))
      }
      val result = parser.parse(tokens)(using context)

      result shouldBe Right(SExpressionNode(IntegerValueExpressionNode(42)))
    }

    it("should parse a name token to an SExpressionNode with a SymbolExpressionNode") {
      val tokens = PeekingIterator(Seq(QuoteToken, NameToken("mySymbol")).iterator)
      val parser = new SExpressionNodeParser {}
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] = ???}

      val result = parser.parse(tokens)(using context)

      result shouldBe Right(SExpressionNode(SymbolExpressionNode("mySymbol")))
    }

    it("should parse a parenthesized list of tokens into an SExpressionNode containing a sequence of SExpressionNodes") {
      val tokens = PeekingIterator(Seq(
        QuoteToken,
        LeftParenthesisToken,
        NameToken("a"),
        IntegerValueToken("1"),
        RightParenthesisToken
      ).iterator)
      val parser = new SExpressionNodeParser {}
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          if tokens.peek(1) == Seq(IntegerValueToken("1")) then
            tokens.consumeTokens(1)
            Right(IntegerValueExpressionNode(IntegerValue(1)))
          else
            Left("Unexpect stuff")
      }

      val result = parser.parse(tokens)(using context)

      result should matchPattern {
        case Right(SExpressionNode(Seq(SExpressionNode(SymbolExpressionNode("a")), SExpressionNode(IntegerValueExpressionNode(IntegerValue(1)))))) =>
      }
    }

    it("should fail if the quote token is not followed by a valid token type") {
      val tokens = PeekingIterator(Seq(QuoteToken, RightParenthesisToken).iterator)
      val parser = new SExpressionNodeParser {}
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] = ???}

      val result = parser.parse(tokens)(using context)

      result shouldBe a[Left[_, _]]
      result.left.get should include("' is an unexpected token")
    }

    it("should fail if an integer token does not parse to an IntegerValueExpressionNode") {
      val tokens = PeekingIterator(Seq(QuoteToken, IntegerValueToken("42")).iterator)
      val parser = new SExpressionNodeParser {}
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          tokens.consumeTokens(1)
          Right(new ExpressionNode {})
      }

      val result = parser.parse(tokens)(using context)

      result shouldBe Left("Integer did not parse to integer value expression in parsing S-expression")
    }

    it("should fail if a parenthesized list is not terminated with a right parenthesis") {
      val tokens = PeekingIterator(Seq(
        QuoteToken,
        LeftParenthesisToken,
        NameToken("a"),
        IntegerValueToken("1")
      ).iterator) // No RightParenthesisToken
      val parser = new SExpressionNodeParser {}
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          if tokens.peek(1) == Seq(IntegerValueToken("1")) then
            tokens.consumeTokens(1)
            Right(IntegerValueExpressionNode(IntegerValue(1)))
          else
            Left("Unexpect stuff")
      }

      val result = parser.parse(tokens)(using context)

      result shouldBe a[Left[_, _]]
      result.left.get should include("Invalid end of program")
    }

    it("should delegate to the superclass parse method for unmatched cases") {
      val tokens = PeekingIterator(Seq(new Token{override def literal: String = ???}).iterator)
      val parser = new SExpressionNodeParser {
        override def parse(tokens: PeekingIterator[kamin.Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
          Left("Superclass parse called")
      }
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] = ???
      }

      val result = parser.parse(tokens)(using context)

      result shouldBe Left("Superclass parse called")
    }
  }
}
