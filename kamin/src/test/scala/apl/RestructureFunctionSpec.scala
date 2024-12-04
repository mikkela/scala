package kamin.apl

import kamin.Value
import kamin.IntegerValue
import kamin.apl.{MatrixValue, VectorValue}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps

class RestructureFunctionSpec extends AnyFunSpec with Matchers {

  describe("Restructuring Vector") {
    it("should restructure to IntegerValue for a shape of length 0") {
      val result = restructuring(VectorValue.emptyVector, VectorValue.createVector(Seq(1, 2, 3)))
      result shouldEqual Right(IntegerValue(1))
    }

    it("should restructure to VectorValue for a shape of length 1") {
      val result = restructuring(VectorValue.createVector(Seq(3)), VectorValue.createVector(Seq(1, 2, 3)))
      result shouldEqual Right(VectorValue.createVector(Vector(1, 2, 3)))
    }

    it("should restructure to VectorValue for a shape of length 1 cutting of if too long") {
      val result = restructuring(VectorValue.createVector(Seq(2)), VectorValue.createVector(Seq(1, 2, 3)))
      result shouldEqual Right(VectorValue.createVector(Vector(1, 2)))
    }

    it("should restructure to VectorValue for a shape of length 1 filling in if too short") {
      val result = restructuring(VectorValue.createVector(Seq(5)), VectorValue.createVector(Seq(1, 2, 3)))
      result shouldEqual Right(VectorValue.createVector(Vector(1, 2, 3, 1, 2)))
    }

    it("should restructure to MatrixValue for a shape of length 2") {
      val result = restructuring(VectorValue.createVector(Seq(2, 3)), VectorValue.createVector(Seq(1, 2, 3, 4, 5, 6)))
      result shouldEqual Right(MatrixValue.createMatrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6))))
    }

    it("should restructure to MatrixValue for a shape of length 2 cutting off if too long") {
      val result = restructuring(VectorValue.createVector(Seq(2, 3)), VectorValue.createVector(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)))
      result shouldEqual Right(MatrixValue.createMatrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6))))
    }

    it("should restructure to MatrixValue for a shape of length 2 filling in if too short") {
      val result = restructuring(VectorValue.createVector(Seq(2, 3)), VectorValue.createVector(Seq(1, 2, 3, 4)))
      result shouldEqual Right(MatrixValue.createMatrix(Vector(Vector(1, 2, 3), Vector(4, 1, 2))))
    }
  }

  describe("Restructuring Matrix") {
    it("should restructure to IntegerValue for a shape of length 0") {
      val result = restructuring(VectorValue.createVector(Vector(0)), MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4))))
      result shouldEqual Right(IntegerValue(1))
    }

    it("should restructure to VectorValue for a shape of length 1") {
      val result = restructuring(VectorValue.createVector(Vector(4)), MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4))))
      result shouldEqual Right(VectorValue.createVector(Vector(1, 2, 3, 4)))
    }

    it("should restructure to VectorValue for a shape of length 1 cutting off if too long") {
      val result = restructuring(VectorValue.createVector(Vector(3)), MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4))))
      result shouldEqual Right(VectorValue.createVector(Vector(1, 2, 3)))
    }

    it("should restructure to VectorValue for a shape of length 1 filling in if too short") {
      val result = restructuring(VectorValue.createVector(Vector(7)), MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4))))
      result shouldEqual Right(VectorValue.createVector(Vector(1, 2, 3, 4, 1, 2, 3)))
    }

    it("should restructure to MatrixValue for a shape of length 2") {
      val result = restructuring(VectorValue.createVector(Vector(2, 2)), MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4))))
      result shouldEqual Right(MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4))))
    }

    it("should restructure to MatrixValue for a shape of length 2 cutting off if too long") {
      val result = restructuring(VectorValue.createVector(Vector(2, 2)), MatrixValue.createMatrix(Vector(Vector(1, 2, 3), Vector(3, 4, 5))))
      result shouldEqual Right(MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 3))))
    }

    it("should restructure to MatrixValue for a shape of length 2 filling in if too short") {
      val result = restructuring(VectorValue.createVector(Vector(4, 4)), MatrixValue.createMatrix(Vector(Vector(1, 2, 3), Vector(3, 4, 5))))
      result shouldEqual Right(MatrixValue.createMatrix(Vector(Vector(1, 2, 3, 3), Vector(4, 5, 1, 2), Vector(3, 3, 4, 5), Vector(1, 2, 3, 3))))
    }
  }

  describe("restructuring Integer") {
    it("should return IntegerValue for a scalar value and shape of length 0") {
      val shape = VectorValue.createVector(Vector(0))
      val result = restructuring(shape, IntegerValue(5))
      result shouldEqual Right(IntegerValue(5))
    }

    it("should return VectorValue for a vector value and shape of length 1") {
      val shape = VectorValue.createVector(Vector(3))
      val result = restructuring(shape, IntegerValue(4))
      result shouldEqual Right(VectorValue.createVector(Vector(4, 4, 4)))
    }

    it("should return MatrixValue for a matrix value and shape of length 2") {
      val shape = VectorValue.createVector(Vector(2, 3))
      val result = restructuring(shape, IntegerValue(2))
      result shouldEqual Right(MatrixValue.createMatrix(Vector(Vector(2, 2, 2), Vector(2, 2, 2))))
    }
  }
  describe("restructuring") {
    it("should return Left with error message for invalid shape vector length") {
      val shape = VectorValue.createVector(Vector(4, 5, 6)) // Invalid shape length
      val result = restructuring(shape, VectorValue.createVector(Vector(1, 2, 3)))
      result shouldEqual Left("A shape vector is a Vector of length 0, 1 or 2")
    }

    it("should return Left with error message for unsupported type in restructuring") {
      val shape = VectorValue.createVector(Vector(3))
      val result = restructuring(shape, new Value {
        override def isTrue: Boolean = ???
      }) // Unsupported type
      result shouldEqual Left("Invalid parameters")
    }
  }
}
