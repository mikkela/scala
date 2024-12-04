package kamin.apl

import kamin.IntegerValue
import org.scalatest.funspec.AnyFunSpec

class SubscriptingSpec extends AnyFunSpec {

  describe("subscripting") {

    it("should return a VectorValue when subscripting a VectorValue with valid indices") {
      val vector = VectorValue.createVector(Vector(10, 20, 30, 40, 50))
      val indices = VectorValue.createVector(Vector(1, 3, 5))

      val result = subscripting(vector, indices)
      assert(result == Right(VectorValue.createVector(Vector(10, 30, 50))))
    }

    it("should return an error when subscripting a VectorValue with invalid indices") {
      val vector = VectorValue.createVector(Vector(10, 20, 30, 40, 50))
      val indices = VectorValue.createVector(Vector(0, 6))

      val result = subscripting(vector, indices)
      assert(result == Left("Invalid subscripts"))
    }

    it("should return a MatrixValue when subscripting a MatrixValue with valid indices") {
      val matrix = MatrixValue.createMatrix(Vector(
        Vector(1, 2, 3),
        Vector(4, 5, 6),
        Vector(7, 8, 9)
      ))
      val indices = VectorValue.createVector(Vector(1, 3))

      val result = subscripting(matrix, indices)
      assert(result == Right(MatrixValue.createMatrix(Vector(
        Vector(1, 2, 3),
        Vector(7, 8, 9)
      ))))
    }

    it("should return an error when subscripting a MatrixValue with invalid indices") {
      val matrix = MatrixValue.createMatrix(Vector(
        Vector(1, 2, 3),
        Vector(4, 5, 6),
        Vector(7, 8, 9)
      ))
      val indices = VectorValue.createVector(Vector(0, 4))

      val result = subscripting(matrix, indices)
      assert(result == Left("Invalid subscripts"))
    }

    it("should return an error for unsupported value types") {
      val integer = kamin.IntegerValue(42)
      val indices = VectorValue.createVector(Vector(1, 2))

      val result = subscripting(integer, indices)
      assert(result == Left("Invalid parameters"))
    }

    it("should handle an empty indices VectorValue correctly") {
      val vector = VectorValue.createVector(Vector(10, 20, 30, 40, 50))
      val indices = VectorValue.createVector(Vector.empty)

      val result = subscripting(vector, indices)
      assert(result == Right(VectorValue.createVector(Vector.empty)))
    }

    it("should handle an empty MatrixValue correctly with valid indices") {
      val matrix = MatrixValue.createMatrix(Vector.empty)
      val indices = VectorValue.createVector(Vector(1, 2))

      val result = subscripting(matrix, indices)
      assert(result == Left("Invalid subscripts"))
    }
  }
}
