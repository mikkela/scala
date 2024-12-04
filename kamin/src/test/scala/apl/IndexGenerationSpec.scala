package kamin.apl

import kamin.IntegerValue
import org.scalatest.funspec.AnyFunSpec

class IndexGenerationSpec extends AnyFunSpec {

  describe("indexGeneration") {
    it("should generate a VectorValue of integers from 1 to n for a positive IntegerValue") {
      val index = IntegerValue(5)

      val result = indexGeneration(index)
      assert(result == Right(VectorValue.createVector(Seq(1, 2, 3, 4, 5))))
    }

    it("should return a VectorValue with a single element when the IntegerValue is 1") {
      val index = IntegerValue(1)

      val result = indexGeneration(index)
      assert(result == Right(VectorValue.createVector(Seq(1))))
    }

    it("should return an empty VectorValue when the IntegerValue is 0") {
      val index = IntegerValue(0)

      val result = indexGeneration(index)
      assert(result == Right(VectorValue.createVector(Seq.empty[Int])))
    }

    it("should return an error when the IntegerValue is negative") {
      val index = IntegerValue(-3)

      val result = indexGeneration(index)
      assert(result.isRight)
      assert(result == Right(VectorValue.createVector(Seq.empty[Int])))
    }
  }
}
