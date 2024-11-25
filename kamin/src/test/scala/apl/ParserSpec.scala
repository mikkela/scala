package kamin.apl

import kamin.{IntegerValueToken, LeftParenthesisToken, NameToken, QuoteToken, RightParenthesisToken, Token}
import kamin.ExpressionNode
import kamin.PeekingIterator
import kamin.BasicLanguageFamilyParserContext
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

class ParserSpec extends AnyFunSpec
  with Matchers
  with MockitoSugar {
  describe("A vector value expression node parser") {

    it("should return a vector value expression node when presented with a valid vector") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(
        Seq(QuoteToken, LeftParenthesisToken, IntegerValueToken("1"), IntegerValueToken("2"), IntegerValueToken("3"), RightParenthesisToken).iterator
      )
      val sut = new VectorValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Right(
        VectorValueExpressionNode(VectorValue.createVector(Vector(1, 2, 3)).getOrElse(VectorValue.emptyVector))
      )
    }

    it("should return a vector with an empty vector value when presented with an empty list") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(
        Seq(QuoteToken, LeftParenthesisToken, RightParenthesisToken).iterator
      )
      val sut = new VectorValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Right(
        VectorValueExpressionNode(VectorValue.emptyVector)
      )
    }

    it("should return an error when presented with invalid tokens at the start") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(
        Seq(NameToken("InvalidStart"), LeftParenthesisToken, IntegerValueToken("1"), RightParenthesisToken).iterator
      )
      val sut = new VectorValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Left("InvalidStart is an unexpected token")
    }

    it("should return an error when parentheses are mismatched") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(
        Seq(QuoteToken, LeftParenthesisToken, IntegerValueToken("1")).iterator
      )
      val sut = new VectorValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Left("Invalid end of program")
    }

    it("should return an error when the list contains invalid elements") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(
        Seq(QuoteToken, LeftParenthesisToken, NameToken("InvalidElement"), RightParenthesisToken).iterator
      )
      val sut = new VectorValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Left("InvalidElement is an unexpected token")
    }

    it("should return an error when an invalid token is encountered after a valid start") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(
        Seq(QuoteToken, LeftParenthesisToken, IntegerValueToken("1"), NameToken("Unexpected"), RightParenthesisToken).iterator
      )
      val sut = new VectorValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Left("Unexpected is an unexpected token")
    }
  }

  describe("Maximum expression node parsers") {
    it("should return a maximum expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MaxToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MaximumExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(MaximumExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MaxToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MaximumExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("max requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MaxToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MaximumExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("max requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MaxToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MaximumExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("And expression node parsers") {
    it("should return an and expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AndToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AndExpressionNodeParser  {}

      sut.parse(peekingIterator)(using context) shouldBe Right(AndExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AndToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AndExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("and requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AndToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AndExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("and requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AndToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AndExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Or expression node parsers") {
    it("should return an or expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, OrToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new OrExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(OrExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, OrToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new OrExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("or requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, OrToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new OrExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("or requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, OrToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new OrExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }
  describe("Catenation expression node parsers") {
    it("should return a catenation expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, CatToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new CatenationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(CatenationExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, CatToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new CatenationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("cat requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, CatToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new CatenationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("cat requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, CatToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new CatenationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Compression expression node parsers") {
    it("should return a compression expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, CompressToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new CompressionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(CompressionExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, CompressToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new CompressionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("compress requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, CompressToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new CompressionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("compress requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, CompressToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new CompressionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Restructuring expression node parsers") {
    it("should return a restructuring expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, RestructToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new RestructuringExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(RestructuringExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, RestructToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new RestructuringExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("restruct requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, RestructToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new RestructuringExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("restruct requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, RestructToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new RestructuringExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Subscripting expression node parsers") {
    it("should return a subscripting expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SquareBracketsToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubscriptingExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(SubscriptingExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SquareBracketsToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubscriptingExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("[] requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SquareBracketsToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubscriptingExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("[] requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SquareBracketsToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubscriptingExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Addition Reduction expression node parsers") {
    it("should return a addition reduction expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PlusSlashToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AdditionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(AdditionReductionExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PlusSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AdditionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("+/ requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PlusSlashToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AdditionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("+/ requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PlusSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AdditionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Subtraction Reduction expression node parsers") {
    it("should return a subtraction reduction expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MinusSlashToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubtractionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(SubtractionReductionExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MinusSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubtractionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("-/ requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MinusSlashToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubtractionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("-/ requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MinusSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubtractionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Multiplication Reduction expression node parsers") {
    it("should return a multiplication reduction expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AsteriskSlashToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MultiplicationReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(MultiplicationReductionExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AsteriskSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MultiplicationReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("*/ requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AsteriskSlashToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MultiplicationReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("*/ requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AsteriskSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MultiplicationReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Division Reduction expression node parsers") {
    it("should return a division reduction expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SlashSlashToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new DivisionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(DivisionReductionExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SlashSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new DivisionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("// requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SlashSlashToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new DivisionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("// requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SlashSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new DivisionReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Maximum Reduction expression node parsers") {
    it("should return a maximum reduction expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MaxSlashToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MaximumReductionExpressionNodeParser() {}

      sut.parse(peekingIterator)(using context) shouldBe Right(MaximumReductionExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MaxSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MaximumReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("max/ requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MaxSlashToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MaximumReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("max/ requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MaxSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MaximumReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("And Reduction expression node parsers") {
    it("should return a and reduction expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AndSlashToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AndReductionExpressionNodeParser() {}

      sut.parse(peekingIterator)(using context) shouldBe Right(AndReductionExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AndSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AndReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("and/ requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AndSlashToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AndReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("and/ requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AndSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AndReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Or Reduction expression node parsers") {
    it("should return a or reduction expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, OrSlashToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new OrReductionExpressionNodeParser() {}

      sut.parse(peekingIterator)(using context) shouldBe Right(OrReductionExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, OrSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new OrReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("or/ requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, OrSlashToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new OrReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("or/ requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, OrSlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new OrReductionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Shape expression node parsers") {
    it("should return a shape expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, ShapeToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new ShapeExpressionNodeParser() {}

      sut.parse(peekingIterator)(using context) shouldBe Right(ShapeExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, ShapeToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new ShapeExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("shape requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, ShapeToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new ShapeExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("shape requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, ShapeToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new ShapeExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Raveling expression node parsers") {
    it("should return a raveling expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, RavelToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new RavelingExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(RavelingExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, RavelToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new RavelingExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("ravel requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, RavelToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new RavelingExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("ravel requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, RavelToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new RavelingExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Index expression node parsers") {
    it("should return a index expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IndxToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new IndexGenerationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(IndexGenerationExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IndxToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new IndexGenerationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("indx requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IndxToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new IndexGenerationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("indx requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IndxToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new IndexGenerationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Transposition expression node parsers") {
    it("should return a transposition expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, TransToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new TranspositionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(TranspositionExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, TransToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new TranspositionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("trans requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {

      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, TransToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new TranspositionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("trans requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, TransToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new TranspositionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }
}
