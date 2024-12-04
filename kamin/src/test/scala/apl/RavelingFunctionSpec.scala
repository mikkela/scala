package kamin.apl

import kamin.IntegerValue
import kamin.Value

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RavelingFunctionSpec extends AnyFunSpec with Matchers {

  describe("raveling function") {

    it("should return a single-element VectorValue when given an IntegerValue") {
      val value = IntegerValue(42)
      raveling(value) shouldEqual Right(VectorValue.createVector(Seq(42)))
    }

    it("should return the same VectorValue when given a VectorValue") {
      val value = VectorValue.createVector(Seq(1, 2, 3))
      raveling(value) shouldEqual Right(VectorValue.createVector(Seq(1, 2, 3)))
    }

    it("should flatten a MatrixValue into a VectorValue") {
      val value = MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4)))
      raveling(value) shouldEqual Right(VectorValue.createVector(Seq(1, 2, 3, 4)))
    }

    it("should return an error when given an unsupported type") {
      val value = new Value:
        override def isTrue: Boolean = ???
      raveling(value) shouldEqual Left("Invalid parameters")
    }
  }
}
