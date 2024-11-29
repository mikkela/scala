package kamin.apl

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import kamin.apl.ValueExtensions.shape
import kamin.{Value, IntegerValue}
class ValueExtensionsSpec extends AnyFunSpec with Matchers {

  describe("ValueExtensions.shape") {

    it("should return an empty vector for IntegerValue") {
      val intValue = IntegerValue(42)
      intValue.shape shouldEqual Right(VectorValue.emptyVector)
    }

    it("should return a vector with the length of a VectorValue") {
      val vector = VectorValue.createVector(Seq(1, 2, 3))
      vector.shape shouldEqual Right(VectorValue.createVector(Seq(3)))
    }

    /*it("should return a vector with the dimensions of a MatrixValue") {
      val matrix = MatrixValue(Seq(Seq(1, 2), Seq(3, 4), Seq(5, 6)))
      matrix.shape shouldEqual Right(VectorValue(Seq(3, 2)))
    }*/

    it("should return an error for unknown types of Value") {
      case class UnknownValue() extends Value {
        override def isTrue: Boolean = ???
      }
      val unknown = UnknownValue()
      unknown.shape shouldEqual Left(s"Unable to determine shape of an unknown type: ${unknown.getClass.getName}")
    }
  }
}
