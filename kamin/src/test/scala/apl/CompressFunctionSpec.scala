package kamin.apl

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CompressFunctionSpec extends AnyFunSpec with Matchers {

  describe("compress function") {

    describe("when compressing a VectorValue with a control VectorValue") {

      it("should return the compressed vector if the control vector is logical and shapes match") {
        val controlVector = VectorValue.createVector(Seq(1, 0, 1))
        val value = VectorValue.createVector(Seq(10, 20, 30))
        compress(controlVector, value) shouldEqual Right(VectorValue.createVector(Seq(10, 30)))
      }

      it("should return an error if the control vector is not logical") {
        val controlVector = VectorValue.createVector(Seq(2, 0, 1))
        val value = VectorValue.createVector(Seq(10, 20, 30))
        compress(controlVector, value) shouldEqual Left("control vector consists of only 0 and 1")
      }

      it("should return an error if the shapes of the vectors do not match") {
        val controlVector = VectorValue.createVector(Seq(1, 0))
        val value = VectorValue.createVector(Seq(10, 20, 30))
        compress(controlVector, value) shouldEqual notOfSameShape
      }
    }

    describe("when compressing a MatrixValue with a control vector") {

      it("should return the compressed matrix if the control vector is logical and column shapes match") {
        val controlVector = VectorValue.createVector(Seq(1, 0, 1))
        val value = MatrixValue.createMatrix(Vector(Vector(10, 20, 30), Vector(40, 50, 60)))
        compress(controlVector, value) shouldEqual Right(MatrixValue.createMatrix(Vector(Vector(10, 30), Vector(40, 60))))
      }

      it("should return an error if the control vector is not logical") {
        val controlVector = VectorValue.createVector(Seq(2, 0, 1))
        val value = MatrixValue.createMatrix(Vector(Vector(10, 20, 30), Vector(40, 50, 60)))
        compress(controlVector, value) shouldEqual Left("Control vector must consist of only 0 and 1")
      }

      it("should return an error if the control vector length does not match the matrix column shape") {
        val controlVector = VectorValue.createVector(Seq(1, 0))
        val value = MatrixValue.createMatrix(Vector(Vector(10, 20, 30), Vector(40, 50, 60)))
        compress(controlVector, value) shouldEqual notOfSameShape
      }
    }
  }
}