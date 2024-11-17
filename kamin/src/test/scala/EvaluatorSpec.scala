import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar

class EvaluatorSpec extends AnyFunSpec
  with Matchers
  with MockitoSugar
  with TableDrivenPropertyChecks {

  describe("evaluate for IntegerValueNode") {
    it("should return the integer value even without an environment") {
      val sut = IntegerExpressionNode(100)

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using new FunctionDefinitionTable {}) shouldBe Right(100)
    }
  }

  describe("evaluate for VariableNode") {
    it("should return the evaluated expression registered at the variable") {
      val environment = GlobalAndLocalScopeEnvironment()
      environment.set("x", 20)
      val sut = VariableExpressionNode("x")

      sut.evaluate(using environment)(using new FunctionDefinitionTable{}) shouldBe Right(20)
    }

    it("should return an error if the variable does not exists") {
      val sut = VariableExpressionNode("x")

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using new FunctionDefinitionTable{}) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluate for IfExpressionNode") {
    it("should return the second expression if the evaluation of the first is non zero") {
      val sut = IfExpressionNode(IntegerExpressionNode(1), IntegerExpressionNode(2), IntegerExpressionNode(3))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using new FunctionDefinitionTable{}) shouldBe Right(2)
    }

    it("should return the third expression if the evaluation of the first is zero") {
      val sut = IfExpressionNode(IntegerExpressionNode(0), IntegerExpressionNode(2), IntegerExpressionNode(3))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using new FunctionDefinitionTable{}) shouldBe Right(3)
    }

    it("should return the the error if the evaluation of the first an error") {
      val sut = IfExpressionNode(VariableExpressionNode("x"), IntegerExpressionNode(2), IntegerExpressionNode(3))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using new FunctionDefinitionTable{}) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluate for SetExpressionNode") {
    it("should register the evaluation result in the environment and return it as well") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = SetExpressionNode("foo", IntegerExpressionNode(265))

      sut.evaluate(using env)(using new FunctionDefinitionTable{}) shouldBe Right(265)
      env.get("foo") shouldBe Some(265)
    }

    it("should return the the error if the evaluation of the value expression returns an error") {
      val sut = SetExpressionNode("wrong", VariableExpressionNode("x"))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using new FunctionDefinitionTable{}) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluate for WhileExpressionNode") {
    it("should only evaluate the test and not the body if the test returns 0") {
      val sut = WhileExpressionNode(IntegerExpressionNode(0), VariableExpressionNode("x"))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using new FunctionDefinitionTable{}) shouldBe Right(0)
    }

    it("should only evaluate the body if the test returns non-zero") {
      val env = GlobalAndLocalScopeEnvironment()
      env.set("x", 1)
      val sut = WhileExpressionNode(VariableExpressionNode("x"), SetExpressionNode("x", IntegerExpressionNode(0)))

      sut.evaluate(using env)(using new FunctionDefinitionTable{}) shouldBe Right(0)
      env.get("x") shouldBe Some(0)
    }

    it("should return the the error if the evaluation of the test expression returns an error") {
      val sut = WhileExpressionNode(VariableExpressionNode("x"), IntegerExpressionNode(100))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using new FunctionDefinitionTable{}) shouldBe Left("x is not recognized")
    }

    it("should return the the error if the evaluation of the body expression returns an error") {
      val sut = WhileExpressionNode(IntegerExpressionNode(100), VariableExpressionNode("x"))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using new FunctionDefinitionTable{}) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluate for BeginExpressionNode") {
    it("should return the result of the last expression after evaluating them all") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = BeginExpressionNode(Seq(
        SetExpressionNode("x", IntegerExpressionNode(2)),
        SetExpressionNode("y", IntegerExpressionNode(25)),
        IntegerExpressionNode(123)))

      sut.evaluate(using env)(using new FunctionDefinitionTable{}) shouldBe Right(123)
      env.get("x") shouldBe Some(2)
      env.get("y") shouldBe Some(25)
    }

    it("should return the the error if one of the expression returns an error") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = BeginExpressionNode(Seq(
        SetExpressionNode("x", IntegerExpressionNode(2)),
        VariableExpressionNode("y"),
        IntegerExpressionNode(123)))

      sut.evaluate(using env)(using new FunctionDefinitionTable{}) shouldBe Left("y is not recognized")
      env.get("x") shouldBe Some(2)
    }
  }

  describe("evaluate for FunctionCallExpressionNode") {
    it("should return the result of the function body when called") {
      val env = GlobalAndLocalScopeEnvironment()
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            IntegerExpressionNode(300),
            IntegerExpressionNode(400),
            IntegerExpressionNode(500)))))

      val sut = FunctionCallExpressionNode("foo", Seq(
        IntegerExpressionNode(10),
        IntegerExpressionNode(20)))
      sut.evaluate(using env)(using table) shouldBe Right(500)
    }

    it("should remove the local scope environment afterwards") {
      val env = GlobalAndLocalScopeEnvironment()
      env.set("a", 2)
      env.set("b", 3)
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            SetExpressionNode("a", VariableExpressionNode("x")),
            SetExpressionNode("b", VariableExpressionNode("y")),
            IntegerExpressionNode(500)))))

      val sut = FunctionCallExpressionNode("foo", Seq(
        IntegerExpressionNode(10),
        IntegerExpressionNode(20)))
      sut.evaluate(using env)(using table)
      env.get("x") shouldBe None
      env.get("y") shouldBe None
    }

    it("should change the local scope only if exists and then the global next") {
      val env = GlobalAndLocalScopeEnvironment()
      env.set("a", 2)
      env.set("b", 3)
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
        IntegerExpressionNode(10),
        IntegerExpressionNode(20)))
      sut.evaluate(using env)(using table) shouldBe Right(30)
      env.get("a") shouldBe Some(10)
      env.get("b") shouldBe Some(20)
    }
    
    it("should return the the error if one of the parameter evaluations returns an error") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = FunctionCallExpressionNode("foo", Seq(
        VariableExpressionNode("y"),
        IntegerExpressionNode(123)))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            SetExpressionNode("a", VariableExpressionNode("x")),
            SetExpressionNode("b", VariableExpressionNode("y")),
            IntegerExpressionNode(500)))))
      sut.evaluate(using env)(using table) shouldBe Left("y is not recognized")
    }

    it("should return the the error if the function call have too few parameters") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = FunctionCallExpressionNode("foo", Seq(IntegerExpressionNode(123)))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            VariableExpressionNode("x"),
            VariableExpressionNode("y")))))
      sut.evaluate(using env)(using table) shouldBe Left("foo requires 2 arguments")
    }

    it("should return the the error if the function call have too many parameters") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = FunctionCallExpressionNode("foo",
        Seq(IntegerExpressionNode(123), IntegerExpressionNode(234), IntegerExpressionNode(542)))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x", "y"),
          BeginExpressionNode(Seq(
            VariableExpressionNode("x"),
            VariableExpressionNode("y")))))
      sut.evaluate(using env)(using table) shouldBe Left("foo requires 2 arguments")
    }

    it("should return the the error if the function evaluation fails") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = FunctionCallExpressionNode("foo", Seq(IntegerExpressionNode(123)))
      val table = new FunctionDefinitionTable {}
      table.register(
        FunctionDefinitionNode(
          "foo",
          Seq("x"),
          BeginExpressionNode(Seq(
            VariableExpressionNode("x"),
            VariableExpressionNode("y")))))
      sut.evaluate(using env)(using table) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluate for AdditionExpressionNode") {
    it("should return the result of addition of the operands when called") {
      val sut = AdditionExpressionNode(IntegerExpressionNode(10), IntegerExpressionNode(20))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(30)
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = AdditionExpressionNode(IntegerExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluate for SubtractionExpressionNode") {
    it("should return the result of subtraction of the operands when called") {
      val sut = SubtractionExpressionNode(IntegerExpressionNode(30), IntegerExpressionNode(20))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(10)
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = SubtractionExpressionNode(IntegerExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluate for MultiplicationExpressionNode") {
    it("should return the result of multiplication of the operands when called") {
      val sut = MultiplicationExpressionNode(IntegerExpressionNode(30), IntegerExpressionNode(20))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(600)
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = MultiplicationExpressionNode(IntegerExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluate for DivisionExpressionNode") {
    it("should return the result of division of the operands when called") {
      val sut = DivisionExpressionNode(IntegerExpressionNode(40), IntegerExpressionNode(20))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(2)
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = DivisionExpressionNode(IntegerExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluate for EqualityExpressionNode") {
    it("should return 1 if the operands are equal") {
      val sut = EqualityExpressionNode(IntegerExpressionNode(40), IntegerExpressionNode(40))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(1)
    }

    it("should return 0 if the operands are not equal") {
      val sut = EqualityExpressionNode(IntegerExpressionNode(40), IntegerExpressionNode(50))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(0)
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = EqualityExpressionNode(IntegerExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluate for LessThenExpressionNode") {
    it("should return 0 if the first operand is greater than the second") {
      val sut = LessThanExpressionNode(IntegerExpressionNode(70), IntegerExpressionNode(40))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(0)
    }

    it("should return 1 if the first operand is smaller than the second") {
      val sut = LessThanExpressionNode(IntegerExpressionNode(10), IntegerExpressionNode(50))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(1)
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = LessThanExpressionNode(IntegerExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluate for GreaterThenExpressionNode") {
    it("should return 1 if the first operand is greater than the second") {
      val sut = GreaterThanExpressionNode(IntegerExpressionNode(70), IntegerExpressionNode(40))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(1)
    }

    it("should return 0 if the first operand is smaller than the second") {
      val sut = GreaterThanExpressionNode(IntegerExpressionNode(10), IntegerExpressionNode(50))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(0)
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = GreaterThanExpressionNode(IntegerExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluate for PrintExpressionNode") {
    it("should return the result of printing of the operand when called") {
      val sut = PrintExpressionNode(IntegerExpressionNode(400))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Right(400)
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = PrintExpressionNode(VariableExpressionNode("y"))
      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable()) shouldBe Left("y is not recognized")
    }
  }
}
