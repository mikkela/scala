package kamin.apl

import kamin.{AdditionExpressionNode, SubtractionExpressionNode, MultiplicationExpressionNode, DivisionExpressionNode, Arithmetic, FunctionDefinitionTable, GlobalAndLocalScopeEnvironment, IntegerValue, IntegerValueExpressionNode, Reader}
import kamin.given_ExpressionEvaluator_AdditionExpressionNode
import kamin.given_ExpressionEvaluator_SubtractionExpressionNode
import kamin.given_ExpressionEvaluator_MultiplicationExpressionNode
import kamin.given_ExpressionEvaluator_DivisionExpressionNode
import kamin.{IntegerValue}
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
    kamin.apl.RegistriesSetup.initialize()

  describe("evaluateExpression for AdditionExpressionNode for IntegerValue and VectorValue") {
    it("should return the result of addition of the operands when called") {
      val sut = AdditionExpressionNode(IntegerValueExpressionNode(10), VectorValueExpressionNode(VectorValue.createVector(Vector(20, 30, 40))))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(30, 40, 50)))
    }

    it("should return empty vector if empty vector is used") {
      val sut = AdditionExpressionNode(IntegerValueExpressionNode(10), VectorValueExpressionNode(VectorValue.emptyVector))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }
  }

  describe("evaluateExpression for AdditionExpressionNode for VectorValue and IntegerValue") {
    it("should return the result of addition of the operands when called") {
      val sut = AdditionExpressionNode(VectorValueExpressionNode(VectorValue.createVector(Vector(25, 32, 41))), IntegerValueExpressionNode(10))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(35, 42, 51)))
    }

    it("should return empty vector if empty vector is used") {
      val sut = AdditionExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), IntegerValueExpressionNode(10))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }
  }

  describe("evaluateExpression for AdditionExpressionNode for VectorValue and VectorValue") {
    it("should return the result of addition of the operands when called") {
      val v1 = VectorValue.createVector(Vector(25, 32, 41))
      val v2 = VectorValue.createVector(Vector(37, 12, 15))
      val sut = AdditionExpressionNode(VectorValueExpressionNode(v1), VectorValueExpressionNode(v2))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(62, 44, 56)))
    }

    it("should return empty vector if both empty vectors") {
      val sut = AdditionExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), VectorValueExpressionNode(VectorValue.emptyVector))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }

    it("should return an error if different shapes") {
      val v1 = VectorValue.createVector(Vector(25, 32, 41, 67))
      val v2 = VectorValue.createVector(Vector(37, 12, 15))
      val sut = AdditionExpressionNode(VectorValueExpressionNode(v1), VectorValueExpressionNode(v2))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("The two operands are not of same shape")
    }

    it("should return an error if one empty vectors") {
      val v = VectorValue.createVector(Vector(25, 32, 41))
      val sut = AdditionExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), VectorValueExpressionNode(v))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("The two operands are not of same shape")
    }
  }

  describe("evaluateExpression for SubtractionExpressionNode for IntegerValue and VectorValue") {
    it("should return the result of subtracting of the operands when called") {
      val sut = SubtractionExpressionNode(IntegerValueExpressionNode(10), VectorValueExpressionNode(VectorValue.createVector(Vector(20, 30, 40))))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(-10, -20, -30)))
    }

    it("should return empty vector if empty vector is used") {
      val sut = SubtractionExpressionNode(IntegerValueExpressionNode(10), VectorValueExpressionNode(VectorValue.emptyVector))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }
  }

  describe("evaluateExpression for SubtractionExpressionNode for VectorValue and IntegerValue") {
    it("should return the result of subtraction of the operands when called") {
      val sut = SubtractionExpressionNode(VectorValueExpressionNode(VectorValue.createVector(Vector(25, 32, 41))), IntegerValueExpressionNode(10))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(15, 22, 31)))
    }

    it("should return empty vector if empty vector is used") {
      val sut = SubtractionExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), IntegerValueExpressionNode(10))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }
  }

  describe("evaluateExpression for SubtractionExpressionNode for VectorValue and VectorValue") {
    it("should return the result of subtracting of the operands when called") {
      val v1 = VectorValue.createVector(Vector(25, 32, 41))
      val v2 = VectorValue.createVector(Vector(37, 12, 15))
      val sut = SubtractionExpressionNode(VectorValueExpressionNode(v1), VectorValueExpressionNode(v2))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(-12, 20, 26)))
    }

    it("should return empty vector if both empty vectors") {
      val sut = SubtractionExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), VectorValueExpressionNode(VectorValue.emptyVector))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }

    it("should return an error if different shapes") {
      val v1 = VectorValue.createVector(Vector(25, 32, 41, 67))
      val v2 = VectorValue.createVector(Vector(37, 12, 15))
      val sut = SubtractionExpressionNode(VectorValueExpressionNode(v1), VectorValueExpressionNode(v2))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("The two operands are not of same shape")
    }

    it("should return an error if one empty vectors") {
      val v = VectorValue.createVector(Vector(25, 32, 41))
      val sut = SubtractionExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), VectorValueExpressionNode(v))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("The two operands are not of same shape")
    }
  }

  describe("evaluateExpression for MultiplicationExpressionNode for IntegerValue and VectorValue") {
    it("should return the result of multiplying of the operands when called") {
      val sut = MultiplicationExpressionNode(IntegerValueExpressionNode(10), VectorValueExpressionNode(VectorValue.createVector(Vector(20, 30, 40))))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(200, 300, 400)))
    }

    it("should return empty vector if empty vector is used") {
      val sut = MultiplicationExpressionNode(IntegerValueExpressionNode(10), VectorValueExpressionNode(VectorValue.emptyVector))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }
  }

  describe("evaluateExpression for MultiplicationExpressionNode for VectorValue and IntegerValue") {
    it("should return the result of multiplication of the operands when called") {
      val sut = MultiplicationExpressionNode(VectorValueExpressionNode(VectorValue.createVector(Vector(25, 32, 41))), IntegerValueExpressionNode(10))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(250, 320, 410)))
    }

    it("should return empty vector if empty vector is used") {
      val sut = MultiplicationExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), IntegerValueExpressionNode(10))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }
  }

  describe("evaluateExpression for MultiplicationExpressionNode for VectorValue and VectorValue") {
    it("should return the result of subtracting of the operands when called") {
      val v1 = VectorValue.createVector(Vector(2, 3, 4))
      val v2 = VectorValue.createVector(Vector(5, 6, 7))
      val sut = MultiplicationExpressionNode(VectorValueExpressionNode(v1), VectorValueExpressionNode(v2))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(10, 18, 28)))
    }

    it("should return empty vector if both empty vectors") {
      val sut = MultiplicationExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), VectorValueExpressionNode(VectorValue.emptyVector))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }

    it("should return an error if different shapes") {
      val v1 = VectorValue.createVector(Vector(25, 32, 41, 67))
      val v2 = VectorValue.createVector(Vector(37, 12, 15))
      val sut = MultiplicationExpressionNode(VectorValueExpressionNode(v1), VectorValueExpressionNode(v2))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("The two operands are not of same shape")
    }

    it("should return an error if one empty vectors") {
      val v = VectorValue.createVector(Vector(25, 32, 41))
      val sut = MultiplicationExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), VectorValueExpressionNode(v))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("The two operands are not of same shape")
    }
  }

  describe("evaluateExpression for DivisionExpressionNode for IntegerValue and VectorValue") {
    it("should return the result of division of the operands when called") {
      val sut = DivisionExpressionNode(IntegerValueExpressionNode(1200), VectorValueExpressionNode(VectorValue.createVector(Vector(20, 30, 40))))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(60, 40, 30)))
    }

    it("should return an error when called with zero") {
      val sut = DivisionExpressionNode(IntegerValueExpressionNode(200), VectorValueExpressionNode(VectorValue.createVector(Vector(20, 0, 40))))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("Cannot divide with zero")
    }

    it("should return empty vector if empty vector is used") {
      val sut = DivisionExpressionNode(IntegerValueExpressionNode(10), VectorValueExpressionNode(VectorValue.emptyVector))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }
  }

  describe("evaluateExpression for DivisionExpressionNode for VectorValue and IntegerValue") {
    it("should return the result of division of the operands when called") {
      val sut = DivisionExpressionNode(VectorValueExpressionNode(VectorValue.createVector(Vector(20, 30, 40))), IntegerValueExpressionNode(10))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(2, 3, 4)))
    }

    it("should return an error when called with zero") {
      val sut = DivisionExpressionNode(VectorValueExpressionNode(VectorValue.createVector(Vector(20, 30, 40))), IntegerValueExpressionNode(0))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("Cannot divide with zero")
    }

    it("should return empty vector if empty vector is used") {
      val sut = DivisionExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), IntegerValueExpressionNode(10))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }
  }

  describe("evaluateExpression for DivisionExpressionNode for VectorValue and VectorValue") {
    it("should return the result of division of the operands when called") {
      val v1 = VectorValue.createVector(Vector(20, 30, 42))
      val v2 = VectorValue.createVector(Vector(5, 6, 7))
      val sut = DivisionExpressionNode(VectorValueExpressionNode(v1), VectorValueExpressionNode(v2))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.createVector(Vector(4, 5, 6)))
    }

    it("should return empty vector if both empty vectors") {
      val sut = DivisionExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), VectorValueExpressionNode(VectorValue.emptyVector))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(VectorValue.emptyVector)
    }

    it("should return an error when called with zero") {
      val v1 = VectorValue.createVector(Vector(20, 30, 42))
      val v2 = VectorValue.createVector(Vector(5, 6, 0))
      val sut = DivisionExpressionNode(VectorValueExpressionNode(v1), VectorValueExpressionNode(v2))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("Cannot divide with zero")
    }

    it("should return an error if different shapes") {
      val v1 = VectorValue.createVector(Vector(25, 32, 41, 67))
      val v2 = VectorValue.createVector(Vector(37, 12, 15))
      val sut = DivisionExpressionNode(VectorValueExpressionNode(v1), VectorValueExpressionNode(v2))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("The two operands are not of same shape")
    }

    it("should return an error if one empty vectors") {
      val v = VectorValue.createVector(Vector(25, 32, 41))
      val sut = MultiplicationExpressionNode(VectorValueExpressionNode(VectorValue.emptyVector), VectorValueExpressionNode(v))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("The two operands are not of same shape")
    }
  }

  /*

  describe("evaluateExpression for MultiplicationExpressionNode") {
    it("should return the result of multiplication of the operands when called") {
      val sut = MultiplicationExpressionNode(IntegerValueExpressionNode(30), IntegerValueExpressionNode(20))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(IntegerValue(600))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = MultiplicationExpressionNode(IntegerValueExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("y is not recognized")
    }
  }

  describe("evaluateExpression for DivisionExpressionNode") {
    it("should return the result of division of the operands when called") {
      val sut = DivisionExpressionNode(IntegerValueExpressionNode(40), IntegerValueExpressionNode(20))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Right(IntegerValue(2))
    }

    it("should return the the error if one of the parameter evaluations returns an error") {
      val sut = DivisionExpressionNode(IntegerValueExpressionNode(10), VariableExpressionNode("y"))
      sut.evaluateExpression(using GlobalAndLocalScopeEnvironment())(using FunctionDefinitionTable())(using new Reader {}) shouldBe Left("y is not recognized")
    }
  }  */
}
