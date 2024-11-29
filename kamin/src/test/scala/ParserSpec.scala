package kamin

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar

class ParserSpec extends AnyFunSpec
  with Matchers
  with MockitoSugar {
  
  describe("A Parser") {
    it("should return an error when presented with an empty token stream") {
      val sut = new Parser[InputNode, ParserContext] {}

      sut.parse(PeekingIterator[Token](Seq.empty.iterator))(using context = null) shouldBe Left("Invalid end of program")
    }

    it("should return an error when presented with an invalid token as the first part") {
      val sut = new Parser[InputNode, ParserContext] {}

      sut.parse(PeekingIterator[Token](Seq(RightParenthesisToken).iterator))(using context = null) shouldBe Left(") is an unexpected token")
    }
  }

  describe("A fun def node parser") {
    it("should return a fun def node when presented with a valid function definition with multiple arguments") {
      val expression = mock[ExpressionNode]
      val results = Seq(Right(expression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, DefineToken, NameToken("plus"),
        LeftParenthesisToken, NameToken("x"), NameToken("y"), RightParenthesisToken,
        RightParenthesisToken
      ).iterator)
      val sut = new FunctionDefinitionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(FunctionDefinitionNode(
        "plus", Seq("x", "y"), expression))
    }

    it("should return a fun def node when presented with a valid function definition with a single argument") {
      val expression = mock[ExpressionNode]
      val results = Seq(Right(expression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, DefineToken, NameToken("not"),
        LeftParenthesisToken, NameToken("x"), RightParenthesisToken,
        RightParenthesisToken
      ).iterator)
      val sut = new FunctionDefinitionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(FunctionDefinitionNode(
        "not", Seq("x"), expression))
    }

    it("should return a fun def node when presented with a valid function definition with no arguments") {
      val expression = mock[ExpressionNode]
      val results = Seq(Right(expression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, DefineToken, NameToken("random"),
        LeftParenthesisToken, RightParenthesisToken,
        RightParenthesisToken
      ).iterator)
      val sut = new FunctionDefinitionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(FunctionDefinitionNode(
        "random", Seq.empty, expression))
    }

    it("should return an error when presented with an fundef construction not closed") {
      val expression = mock[ExpressionNode]
      val results = Seq(Right(expression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, DefineToken, NameToken("foo"),
        LeftParenthesisToken, RightParenthesisToken
      ).iterator)
      val sut = new FunctionDefinitionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Invalid end of program")
    }

    it("should return an error when presented with an fundef construction not parsing the expression correctly") {
      val results = Seq(Left("Failed due to problem in expression"), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, DefineToken, NameToken("foo"),
        LeftParenthesisToken, RightParenthesisToken,
        PlusToken
      ).iterator)
      val sut = new FunctionDefinitionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in expression")
    }

    it("should return an error when presented with an fundef construction with invalid token as argument") {
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, DefineToken, NameToken("foo"),
        LeftParenthesisToken, PlusToken,
      ).iterator)
      val sut = new FunctionDefinitionNodeParser {}

      sut.parse(peekingIterator)(using null) shouldBe Left("+ is an unexpected token")
    }

    it("should return an error when presented with an fundef construction with invalid token as name") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(Seq(
        LeftParenthesisToken, DefineToken, MinusToken
      ).iterator)
      val sut = new FunctionDefinitionNodeParser {}

      sut.parse(peekingIterator)(using null) shouldBe Left("- is an unexpected token")
    }
  }

  describe("An integer value expression node parser") {
    it("should return a value node expression when presented with a valid integer") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(Seq(IntegerValueToken("96575")).iterator)
      val sut = new IntegerValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Right(IntegerValueExpressionNode(96575))
    }

    it("should return an error when presented with a non-integer") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(Seq(NameToken("HH96575")).iterator)
      val sut = new IntegerValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Left("HH96575 is an unexpected token")
    }
  }

  describe("An variable expression node parser") {
    it("should return a variable node expression when presented with a valid name") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(Seq(NameToken("myFoo")).iterator)

      val sut = new VariableExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Right(VariableExpressionNode("myFoo"))
    }

    it("should return an error when presented with an invalid name") {
      val peekingIterator: PeekingIterator[Token] = PeekingIterator(Seq(IntegerValueToken("96575")).iterator)
      val sut = new VariableExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Left("96575 is an unexpected token")
    }
  }

  describe("An if expression node parser") {
    it("should return a if expression node expression when presented with a valid if construction") {
      val testExpression = mock[ExpressionNode]
      val consequenceExpression = mock[ExpressionNode]
      val alternativeExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(consequenceExpression), Right(alternativeExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IfToken,
        RightParenthesisToken
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(IfExpressionNode(testExpression, consequenceExpression, alternativeExpression))
    }

    it("should return an error when presented with an if construction not closed") {
      val testExpression = mock[ExpressionNode]
      val consequenceExpression = mock[ExpressionNode]
      val alternativeExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(consequenceExpression), Right(alternativeExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IfToken
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Invalid end of program")
    }

    it("should return an error when presented with an if construction not properly closed with correct token") {
      val testExpression = mock[ExpressionNode]
      val consequenceExpression = mock[ExpressionNode]
      val alternativeExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(consequenceExpression), Right(alternativeExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IfToken, PlusToken
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("+ is an unexpected token")
    }

    it("should return an error when the alternative expression is not parsed correctly") {
      val testExpression = mock[ExpressionNode]
      val consequenceExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(consequenceExpression), Left("Failed due to problem in alternative expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IfToken
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in alternative expression")
    }

    it("should return an error when the consequence expression is not parsed correctly") {
      val testExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Left("Failed due to problem in consequence expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IfToken
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in consequence expression")
    }

    it("should return an error when the test expression is not parsed correctly") {
      val results = Seq(Left("Failed due to problem in test expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, IfToken
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in test expression")
    }
  }

  describe("A while expression node parser") {
    it("should return a while expression node expression when presented with a valid while construction") {
      val testExpression = mock[ExpressionNode]
      val bodyExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(bodyExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, WhileToken,
        RightParenthesisToken
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(WhileExpressionNode(testExpression, bodyExpression))
    }

    it("should return an error when presented with an while construction not closed") {
      val testExpression = mock[ExpressionNode]
      val bodyExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(bodyExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, WhileToken
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Invalid end of program")
    }

    it("should return an error when presented with an while construction not properly closed with correct token") {
      val testExpression = mock[ExpressionNode]
      val bodyExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(bodyExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, WhileToken, MinusToken
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("- is an unexpected token")
    }

    it("should return an error when the body expression is not parsed correctly") {
      val testExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Left("Failed due to problem in body expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, WhileToken
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in body expression")
    }

    it("should return an error when the test expression is not parsed correctly") {
      val results = Seq(Left("Failed due to problem in test expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, WhileToken
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in test expression")
    }
  }

  describe("A set expression node parser") {
    it("should return a set expression node expression when presented with a valid set construction") {
      val valueExpression = mock[ExpressionNode]
      val results = Seq(Right(valueExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SetToken,
        NameToken("foo"),
        RightParenthesisToken
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(SetExpressionNode("foo", valueExpression))
    }

    it("should return an error when presented with a set construction not closed") {
      val valueExpression = mock[ExpressionNode]
      val results = Seq(Right(valueExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SetToken,
        NameToken("foo")
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Invalid end of program")
    }

    it("should return an error when presented with an set construction not properly closed with correct token") {
      val valueExpression = mock[ExpressionNode]
      val results = Seq(Right(valueExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SetToken,
        NameToken("foo"),
        AsteriskToken
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("* is an unexpected token")
    }

    it("should return an error when the value expression is not parsed correctly") {
      val results = Seq(Left("Failed due to problem in value expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SetToken,
        NameToken("foo")
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in value expression")
    }

    it("should return an error when the variable part is not parsed correctly") {
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SetToken,
        SlashToken
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using null) shouldBe Left("/ is an unexpected token")
    }
  }

  describe("A begin expression node parser") {
    it("should return a begin expression node expression when presented with a valid begin construction and list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]
      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, BeginToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new BeginExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(BeginExpressionNode(List(expression1, expression2, expression3)))
    }

    it("should return a begin expression node expression when presented with a valid begin construction and single expression") {
      val expression = mock[ExpressionNode]
      val results = Seq(Right(expression)).iterator
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, BeginToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new BeginExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(BeginExpressionNode(List(expression)))
    }

    it("should return a No expressions found with a begin construction and no expressions") {
      val results = Seq().iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, BeginToken,
        RightParenthesisToken
      ).iterator)
      val sut = new BeginExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left(") is an unexpected token")
    }

    it("should return the error when an expression fails") {
      val results = Seq(Right(mock[ExpressionNode]), Left("Something went wrong in parsing"), Right(mock[ExpressionNode])).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, BeginToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val sut = new BeginExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Something went wrong in parsing")
    }
  }

  describe("Addition expression node parsers") {
    it("should return a addition expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PlusToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
          val context = new BasicLanguageFamilyParserContext {
            override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
              peekingIterator.consumeTokens(1)
              results.next()
          }

          val sut = new AdditionExpressionNodeParser {}

          sut.parse(peekingIterator)(using context) shouldBe Right(AdditionExpressionNode(expression1, expression2))
      }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PlusToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AdditionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("+ requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PlusToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AdditionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("+ requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PlusToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new AdditionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
   }
  }

  describe("Subtraction expression node parsers") {
    it("should return a subtraction expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MinusToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubtractionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(SubtractionExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MinusToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubtractionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("- requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MinusToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubtractionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("- requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, MinusToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new SubtractionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Multiplication expression node parsers") {
    it("should return a subtraction expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AsteriskToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MultiplicationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(MultiplicationExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AsteriskToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MultiplicationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("* requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AsteriskToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MultiplicationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("* requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, AsteriskToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new MultiplicationExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Division expression node parsers") {
    it("should return a division expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new DivisionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(DivisionExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SlashToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new DivisionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("/ requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SlashToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new DivisionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("/ requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, SlashToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new DivisionExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Equal expression node parsers") {
    it("should return a equal expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, EqualToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new EqualityExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(EqualityExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, EqualToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new EqualityExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("= requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, EqualToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new EqualityExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("= requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, EqualToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new EqualityExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Less than expression node parsers") {
    it("should return a equal expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, LessThanToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new LessThanExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(LessThanExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, LessThanToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new LessThanExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("< requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, LessThanToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new LessThanExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("< requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, LessThanToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new LessThanExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Greather than expression node parsers") {
    it("should return a greater than expression node when presented with valid list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, GreaterThanToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new GreaterThanExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(GreaterThanExpressionNode(expression1, expression2))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, GreaterThanToken,
        NameToken("eaten"), NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new GreaterThanExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("> requires 2 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, GreaterThanToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new GreaterThanExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("> requires 2 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val expression1 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, GreaterThanToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new GreaterThanExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Print expression node parsers") {
    it("should return a print expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PrintToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new PrintExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(PrintExpressionNode(expression))
    }

    it("should return an error when presented with too many expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]

      val results = Seq(Right(expression1), Right(expression2)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PrintToken,
        NameToken("eaten"), NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new PrintExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("print requires 1 arguments")
    }

    it("should return an error when presented with too few expressions") {
      val results = Seq.empty.iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PrintToken, RightParenthesisToken).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new PrintExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("print requires 1 arguments")
    }

    it("should return an error when one of the expressions fails parsing") {
      val results = Seq(Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, PrintToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new PrintExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }

  describe("Read expression node parsers") {
    it("should return a read expression node when presented with empty list of expressions") {
      val results = Seq().iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, ReadToken,
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new ReadExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(ReadExpressionNode())
    }

    it("should return an error when presented with too many expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, ReadToken,
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new ReadExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("read requires 0 arguments")
    }
  }

  describe("Function call expression node parsers") {
    it("should return a function call expression node when presented with valid list of expressions") {
      val expression = mock[ExpressionNode]

      val results = Seq(Right(expression)).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, NameToken("foo"),
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new FunctionCallExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(FunctionCallExpressionNode("foo", Seq(expression)))
    }


    it("should return an error when one of the expressions fails parsing") {
      val results = Seq(Left("Failed parsing")).iterator

      val peekingIterator = PeekingIterator(Seq(
        LeftParenthesisToken, NameToken("foo"),
        NameToken("eaten"),
        RightParenthesisToken
      ).iterator)
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          peekingIterator.consumeTokens(1)
          results.next()
      }

      val sut = new FunctionCallExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed parsing")
    }
  }
}
