package kamin.apl

import kamin.Value
import kamin.IntegerValue
import org.scalatest.funspec.AnyFunSpec

class TranspositionSpec extends AnyFunSpec {

  describe("transposition") {
    it("should return the same IntegerValue for an IntegerValue input") {
      val value = IntegerValue(42)

      val result = transposition(value)
      assert(result == Right(value))
    }

    it("should return the same VectorValue for a VectorValue input") {
      val value = VectorValue.createVector(Seq(1, 2, 3))

      val result = transposition(value)
      assert(result == Right(value))
    }

    it("should transpose a MatrixValue correctly") {
      val matrix = MatrixValue.createMatrix(Seq(
        Seq(1, 2, 3),
        Seq(4, 5, 6)
      ))
      val expected = MatrixValue.createMatrix(Seq(
        Seq(1, 4),
        Seq(2, 5),
        Seq(3, 6)
      ))

      val result = transposition(matrix)
      assert(result == Right(expected))
    }

    it("should handle an empty MatrixValue correctly") {
      val matrix = MatrixValue.createMatrix(Seq.empty[Seq[Int]])

      val result = transposition(matrix)
      assert(result == Right(MatrixValue.createMatrix(Seq.empty[Seq[Int]])))
    }

    it("should handle a single row MatrixValue correctly") {
      val matrix = MatrixValue.createMatrix(Seq(Seq(1, 2, 3)))
      val expected = MatrixValue.createMatrix(Seq(
        Seq(1),
        Seq(2),
        Seq(3)
      ))

      val result = transposition(matrix)
      assert(result == Right(expected))
    }

    it("should handle a single column MatrixValue correctly") {
      val matrix = MatrixValue.createMatrix(Seq(
        Seq(1),
        Seq(2),
        Seq(3)
      ))
      val expected = MatrixValue.createMatrix(Seq(Seq(1, 2, 3)))

      val result = transposition(matrix)
      assert(result == Right(expected))
    }

    it("should return an error for unsupported Value types") {
      val invalidValue = new Value {
        override def isTrue: Boolean = ???
      }

      val result = transposition(invalidValue)
      assert(result == Left("Invalid parameters"))
    }
  }
}
