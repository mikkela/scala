package kamin.apl

import kamin.Value
import kamin.IntegerValue
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ShapeFunctionSpec extends AnyFunSpec with Matchers {

  describe("shape function") {

    it("should return an empty vector for an IntegerValue") {
      shape(IntegerValue(42)) shouldEqual Right(VectorValue.emptyVector)
    }

    it("should return a vector of length 1 with 0 for an empty VectorValue") {
      shape(VectorValue.emptyVector) shouldEqual Right(VectorValue.createVector(Seq(0)))
    }

    it("should return a vector of length 1 with the length of the vector for a VectorValue") {
      shape(VectorValue.createVector(Seq(1, 2, 3))) shouldEqual Right(VectorValue.createVector(Seq(3)))
    }

    it("should return a vector of length 1 with 0 for an empty MatrixValue") {
      shape(MatrixValue.emptyMatrix) shouldEqual Right(VectorValue.createVector(Seq(0)))
    }

    it("should return a vector of the number of rows and columns for a non-empty MatrixValue") {
      val matrix = MatrixValue.createMatrix(Vector(Vector(1, 2), Vector(3, 4)))
      shape(matrix) shouldEqual Right(VectorValue.createVector(Seq(2, 2)))
    }

    it("should return an error for an unknown value type") {
      val unknownValue = new Value {
        override def isTrue: Boolean = ???
      }
      shape(unknownValue) shouldEqual Left(s"Unable to determine shape of an unknown type: ${unknownValue.getClass.getName}")
    }
  }
}
