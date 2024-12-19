package kamin

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar

class ExpressionEvaluatorSpec extends AnyFunSpec
  with Matchers
  with MockitoSugar
  with BeforeAndAfterAll
  with TableDrivenPropertyChecks {

  override def beforeAll(): Unit =
    RegistriesSetup.initialize()

  describe("evaluateExpression for IntegerValueNode") {
    it("should return the integer value even without an environment") {
      val sut = IntegerValueExpressionNode(100)

      sut.evaluateExpression
        (using GlobalEnvironment)
        (using new FunctionDefinitionTable {})
        (using new Reader {})
        (using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Right(IntegerValue(100))
    }
  }

  describe("evaluateExpression for VariableNode") {
    it("should return the evaluated expression registered at the variable") {
      val environment = GlobalEnvironment
      environment.set("x", IntegerValue(20))
      val sut = VariableExpressionNode("x")

      sut.evaluateExpression
        (using environment)
        (using new FunctionDefinitionTable{})
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Right(IntegerValue(20))
    }

    it("should return an error if the variable does not exists") {
      val sut = VariableExpressionNode("x")

      sut.evaluateExpression
        (using GlobalEnvironment)
        (using new FunctionDefinitionTable{})
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluateExpression for IfExpressionNode") {
    it("should return the second expression if the evaluation of the first is non zero") {
      val sut = IfExpressionNode(IntegerValueExpressionNode(1), IntegerValueExpressionNode(2), IntegerValueExpressionNode(3))

      sut.evaluateExpression
        (using GlobalEnvironment)
        (using new FunctionDefinitionTable{})
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Right(IntegerValue(2))
    }

    it("should return the third expression if the evaluation of the first is zero") {
      val sut = IfExpressionNode(IntegerValueExpressionNode(0), IntegerValueExpressionNode(2), IntegerValueExpressionNode(3))

      sut.evaluateExpression
        (using GlobalEnvironment)
        (using new FunctionDefinitionTable{})
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Right(IntegerValue(3))
    }

    it("should return the the error if the evaluation of the first an error") {
      val sut = IfExpressionNode(VariableExpressionNode("x"), IntegerValueExpressionNode(2), IntegerValueExpressionNode(3))

      sut.evaluateExpression
        (using GlobalEnvironment)
        (using new FunctionDefinitionTable{}) 
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluateExpression for SetExpressionNode") {
    it("should register the evaluation result in the environment and return it as well") {
      val env = GlobalEnvironment
      val sut = SetExpressionNode("foo", IntegerValueExpressionNode(265))

      sut.evaluateExpression
        (using env)
        (using new FunctionDefinitionTable{})
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Right(IntegerValue(265))
      env.get("foo") shouldBe Some(IntegerValue(265))
    }

    it("should return the the error if the evaluation of the value expression returns an error") {
      val sut = SetExpressionNode("wrong", VariableExpressionNode("x"))

      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluateExpression for WhileExpressionNode") {
    it("should only evaluateExpression the test and not the body if the test returns 0") {
      val sut = WhileExpressionNode(IntegerValueExpressionNode(0), VariableExpressionNode("x"))

      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        }) shouldBe Right(IntegerValue(0))
    }

    it("should only evaluateExpression the body if the test returns non-zero") {
      val env = GlobalEnvironment
      env.set("x", IntegerValue(1))
      val sut = WhileExpressionNode(VariableExpressionNode("x"), SetExpressionNode("x", IntegerValueExpressionNode(0)))

      sut.evaluateExpression
        (using env)
        (using new FunctionDefinitionTable{}) 
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        }) shouldBe Right(IntegerValue(0))
      env.get("x") shouldBe Some(IntegerValue(0))
    }

    it("should return the the error if the evaluation of the test expression returns an error") {
      val sut = WhileExpressionNode(VariableExpressionNode("x"), IntegerValueExpressionNode(100))

      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("x is not recognized")
    }

    it("should return the the error if the evaluation of the body expression returns an error") {
      val sut = WhileExpressionNode(IntegerValueExpressionNode(100), VariableExpressionNode("x"))

      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluateExpression for BeginExpressionNode") {
    it("should return the result of the last expression after evaluating them all") {
      val env = GlobalEnvironment
      val sut = BeginExpressionNode(Seq(
        SetExpressionNode("x", IntegerValueExpressionNode(2)),
        SetExpressionNode("y", IntegerValueExpressionNode(25)),
        IntegerValueExpressionNode(123)))

      sut.evaluateExpression
        (using env)
        (using new FunctionDefinitionTable{})
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        }) shouldBe Right(IntegerValue(123))
      env.get("x") shouldBe Some(IntegerValue(2))
      env.get("y") shouldBe Some(IntegerValue(25))
    }

    it("should return the the error if one of the expression returns an error") {
      val env = GlobalEnvironment
      val sut = BeginExpressionNode(Seq(
        SetExpressionNode("x", IntegerValueExpressionNode(2)),
        VariableExpressionNode("y"),
        IntegerValueExpressionNode(123)))

      sut.evaluateExpression
        (using env)
        (using new FunctionDefinitionTable{})
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        }) shouldBe Left("y is not recognized")
      env.get("x") shouldBe Some(IntegerValue(2))
    }
  }

  describe("evaluateExpression for FunctionCallExpressionNode") {
    it("should return the result of the function body when called") {
      val env = GlobalEnvironment
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            IntegerValueExpressionNode(300),
            IntegerValueExpressionNode(400),
            IntegerValueExpressionNode(500)))))

      val sut = FunctionCallExpressionNode("foo", Seq(
        IntegerValueExpressionNode(10),
        IntegerValueExpressionNode(20)))
      sut.evaluateExpression(using env)(using table)(using new Reader {})(using new BooleanDefinition{
        override def falseValue: Value = IntegerValue.False

        override def trueValue: Value = IntegerValue.True
      }) shouldBe Right(IntegerValue(500))
    }

    it("should remove the local scope environment afterwards") {
      val env = GlobalEnvironment
      env.set("a", IntegerValue(2))
      env.set("b", IntegerValue(3))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            SetExpressionNode("a", VariableExpressionNode("x")),
            SetExpressionNode("b", VariableExpressionNode("y")),
            IntegerValueExpressionNode(500)))))

      val sut = FunctionCallExpressionNode("foo", Seq(
        IntegerValueExpressionNode(10),
        IntegerValueExpressionNode(20)))
      sut.evaluateExpression(using env)(using table)(using new Reader {})(using new BooleanDefinition{
        override def falseValue: Value = IntegerValue.False

        override def trueValue: Value = IntegerValue.True
      })
      env.get("x") shouldBe None
      env.get("y") shouldBe None
    }

    it("should change the local scope only if exists and then the global next") {
      val env = GlobalEnvironment
      env.set("a", IntegerValue(2))
      env.set("b", IntegerValue(3))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            SetExpressionNode("a", VariableExpressionNode("x")),
            SetExpressionNode("b", VariableExpressionNode("y")),
            AdditionExpressionNode(VariableExpressionNode("a"), VariableExpressionNode("b"))))))

      val sut = FunctionCallExpressionNode("foo", Seq(
        IntegerValueExpressionNode(10),
        IntegerValueExpressionNode(20)))
      sut.evaluateExpression
        (using env)
        (using table)
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        }) shouldBe Right(IntegerValue(30))
      env.get("a") shouldBe Some(IntegerValue(10))
      env.get("b") shouldBe Some(IntegerValue(20))
    }
    
    it("should return the the error if one of the parameter evaluations returns an error") {
      val env = GlobalEnvironment
      val sut = FunctionCallExpressionNode("foo", Seq(
        VariableExpressionNode("y"),
        IntegerValueExpressionNode(123)))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            SetExpressionNode("a", VariableExpressionNode("x")),
            SetExpressionNode("b", VariableExpressionNode("y")),
            IntegerValueExpressionNode(500)))))
      sut.evaluateExpression
        (using env)
        (using table)
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("y is not recognized")
    }

    it("should return the the error if the function call have too few parameters") {
      val env = GlobalEnvironment
      val sut = FunctionCallExpressionNode("foo", Seq(IntegerValueExpressionNode(123)))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            VariableExpressionNode("x"),
            VariableExpressionNode("y")))))
      sut.evaluateExpression
        (using env)
        (using table)
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("foo requires 2 arguments")
    }

    it("should return the the error if the function call have too many parameters") {
      val env = GlobalEnvironment
      val sut = FunctionCallExpressionNode("foo",
        Seq(IntegerValueExpressionNode(123), IntegerValueExpressionNode(234), IntegerValueExpressionNode(542)))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            VariableExpressionNode("x"),
            VariableExpressionNode("y")))))
      sut.evaluateExpression
        (using env)
        (using table)
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("foo requires 2 arguments")
    }

    it("should return the the error if the function evaluation fails") {
      val env = GlobalEnvironment
      val sut = FunctionCallExpressionNode("foo", Seq(IntegerValueExpressionNode(123)))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x"),
          BeginExpressionNode(Seq(
            VariableExpressionNode("x"),
            VariableExpressionNode("y")))))
      sut.evaluateExpression
        (using env)
        (using table)
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        }) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluateExpression for AdditionExpressionNode") {
    it("should return the result of addition of the operands when called") {
      val sut = AdditionExpressionNode(IntegerValueExpressionNode(10), IntegerValueExpressionNode(20))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Right(IntegerValue(30))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = AdditionExpressionNode(IntegerValueExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluateExpression for SubtractionExpressionNode") {
    it("should return the result of subtraction of the operands when called") {
      val sut = SubtractionExpressionNode(IntegerValueExpressionNode(30), IntegerValueExpressionNode(20))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Right(IntegerValue(10))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = SubtractionExpressionNode(IntegerValueExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluateExpression for MultiplicationExpressionNode") {
    it("should return the result of multiplication of the operands when called") {
      val sut = MultiplicationExpressionNode(IntegerValueExpressionNode(30), IntegerValueExpressionNode(20))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Right(IntegerValue(600))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = MultiplicationExpressionNode(IntegerValueExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluateExpression for DivisionExpressionNode") {
    it("should return the result of division of the operands when called") {
      val sut = DivisionExpressionNode(IntegerValueExpressionNode(40), IntegerValueExpressionNode(20))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        })  shouldBe Right(IntegerValue(2))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = DivisionExpressionNode(IntegerValueExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        })  shouldBe Left("y is not recognized")
    }
  }

  describe("evaluateExpression for EqualityExpressionNode") {
    it("should return 1 if the operands are equal") {
      val sut = EqualityExpressionNode(IntegerValueExpressionNode(40), IntegerValueExpressionNode(40))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        }) shouldBe Right(IntegerValue(1))
    }

    it("should return 0 if the operands are not equal") {
      val sut = EqualityExpressionNode(IntegerValueExpressionNode(40), IntegerValueExpressionNode(50))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        })  shouldBe Right(IntegerValue(0))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = EqualityExpressionNode(IntegerValueExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        })  shouldBe Left("y is not recognized")
    }
  }

  describe("evaluateExpression for LessThenExpressionNode") {
    it("should return 0 if the first operand is greater than the second") {
      val sut = LessThanExpressionNode(IntegerValueExpressionNode(70), IntegerValueExpressionNode(40))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        })  shouldBe Right(IntegerValue(0))
    }

    it("should return 1 if the first operand is smaller than the second") {
      val sut = LessThanExpressionNode(IntegerValueExpressionNode(10), IntegerValueExpressionNode(50))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        })  shouldBe Right(IntegerValue(1))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = LessThanExpressionNode(IntegerValueExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        })  shouldBe Left("y is not recognized")
    }
  }

  describe("evaluateExpression for GreaterThenExpressionNode") {
    it("should return 1 if the first operand is greater than the second") {
      val sut = GreaterThanExpressionNode(IntegerValueExpressionNode(70), IntegerValueExpressionNode(40))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        }) shouldBe Right(IntegerValue(1))
    }

    it("should return 0 if the first operand is smaller than the second") {
      val sut = GreaterThanExpressionNode(IntegerValueExpressionNode(10), IntegerValueExpressionNode(50))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = IntegerValue.False

          override def trueValue: Value = IntegerValue.True
        }) shouldBe Right(IntegerValue(0))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = GreaterThanExpressionNode(IntegerValueExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluateExpression for PrintExpressionNode") {
    it("should return the result of printing of the operand when called") {
      val sut = PrintExpressionNode(IntegerValueExpressionNode(400))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Right(IntegerValue(400))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = PrintExpressionNode(VariableExpressionNode("y"))
      sut.evaluateExpression
        (using GlobalEnvironment)
        (using FunctionDefinitionTable())
        (using new Reader {})(using new BooleanDefinition{
          override def falseValue: Value = ???

          override def trueValue: Value = ???
        }) shouldBe Left("y is not recognized")
    }
  }
}
