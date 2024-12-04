package kamin.apl

import kamin.Value
import kamin.IntegerValue

import org.scalatest.funspec.AnyFunSpec

class CatenationSpec extends AnyFunSpec {

  describe("catenation") {
    it("should concatenate two IntegerValue instances") {
      val value1 = IntegerValue(5)
      val value2 = IntegerValue(10)

      val result = catenation(value1, value2)
      assert(result == Right(VectorValue.createVector(Seq(5, 10))))
    }

    it("should concatenate an IntegerValue and a VectorValue") {
      val value1 = IntegerValue(3)
      val value2 = VectorValue.createVector(Seq(7, 8, 9))

      val result = catenation(value1, value2)
      assert(result == Right(VectorValue.createVector(Seq(3, 7, 8, 9))))
    }

    it("should concatenate two VectorValue instances") {
      val value1 = VectorValue.createVector(Seq(1, 2, 3))
      val value2 = VectorValue.createVector(Seq(4, 5, 6))

      val result = catenation(value1, value2)
      assert(result == Right(VectorValue.createVector(Seq(1, 2, 3, 4, 5, 6))))
    }

    it("should concatenate a MatrixValue and a VectorValue") {
      val value1 = MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4)))
      val value2 = VectorValue.createVector(Seq(5, 6))

      val result = catenation(value1, value2)
      assert(result == Right(VectorValue.createVector(Seq(1, 2, 3, 4, 5, 6))))
    }

    it("should concatenate two MatrixValue instances") {
      val value1 = MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4)))
      val value2 = MatrixValue.createMatrix(Vector(Vector(5, 6), Vector(7, 8)))

      val result = catenation(value1, value2)
      assert(result == Right(VectorValue.createVector(Seq(1, 2, 3, 4, 5, 6, 7, 8))))
    }

    it("should return an error if the first value is invalid") {
      val value1 = new Value {
        override def isTrue: Boolean = ???
      }
      val value2 = IntegerValue(10)

      val result = catenation(value1, value2)
      assert(result.isLeft)
      assert(result.left.getOrElse("") == "Invalid parameters")
    }

    it("should return an error if the second value is invalid") {
      val value1 = IntegerValue(10)
      val value2 = new Value {
        override def isTrue: Boolean = ???
      }

      val result = catenation(value1, value2)
      assert(result.isLeft)
      assert(result.left.getOrElse("") == "Invalid parameters")
    }

    it("should return an error if both values are invalid") {
      val value1 = new Value {
        override def isTrue: Boolean = ???
      }
      val value2 = new Value {
        override def isTrue: Boolean = ???
      }

      val result = catenation(value1, value2)
      assert(result.isLeft)
      assert(result.left.getOrElse("") == "Invalid parameters")
    }
  }
}
