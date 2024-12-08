package kamin.lisp

import kamin.IntegerValue
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RelationalSpec extends AnyFunSpec with Matchers {

  describe("IntegerRelational") {

    it("should return SymbolValue.T for equal integer values") {
      val result = IntegerRelational.equal(IntegerValue(5), IntegerValue(5))
      result shouldBe Right(SymbolValue.T)
    }

    it("should return ListValue.nil for unequal integer values") {
      val result = IntegerRelational.equal(IntegerValue(5), IntegerValue(3))
      result shouldBe Right(ListValue.nil)
    }

    it("should return SymbolValue.T when the first integer is greater than the second") {
      val result = IntegerRelational.greaterThan(IntegerValue(5), IntegerValue(3))
      result shouldBe Right(SymbolValue.T)
    }

    it("should return ListValue.nil when the first integer is not greater than the second") {
      val result = IntegerRelational.greaterThan(IntegerValue(3), IntegerValue(5))
      result shouldBe Right(ListValue.nil)
    }

    it("should return SymbolValue.T when the first integer is less than the second") {
      val result = IntegerRelational.lessThan(IntegerValue(3), IntegerValue(5))
      result shouldBe Right(SymbolValue.T)
    }

    it("should return ListValue.nil when the first integer is not less than the second") {
      val result = IntegerRelational.lessThan(IntegerValue(5), IntegerValue(3))
      result shouldBe Right(ListValue.nil)
    }
  }

  describe("SymbolRelational") {

    it("should return SymbolValue.T for equal symbol values") {
      val result = SymbolRelational.equal(SymbolValue("A"), SymbolValue("A"))
      result shouldBe Right(SymbolValue.T)
    }

    it("should return ListValue.nil for unequal symbol values") {
      val result = SymbolRelational.equal(SymbolValue("A"), SymbolValue("B"))
      result shouldBe Right(ListValue.nil)
    }

    it("should return ListValue.nil for greaterThan comparison of symbol values") {
      val result = SymbolRelational.greaterThan(SymbolValue("A"), SymbolValue("B"))
      result shouldBe Right(ListValue.nil)
    }

    it("should return ListValue.nil for lessThan comparison of symbol values") {
      val result = SymbolRelational.lessThan(SymbolValue("A"), SymbolValue("B"))
      result shouldBe Right(ListValue.nil)
    }
  }

  describe("ListRelational") {

    it("should return SymbolValue.T if both lists are nil") {
      val result = ListRelational.equal(ListValue.nil, ListValue.nil)
      result shouldBe Right(SymbolValue.T)
    }

    it("should return ListValue.nil if at least one of the lists is not nil") {
      val result = ListRelational.equal(ListValue(List(IntegerValue(1))), ListValue.nil)
      result shouldBe Right(ListValue.nil)
    }

    it("should return ListValue.nil even if both lists are equal") {
      val result = ListRelational.equal(ListValue(List(IntegerValue(1))), ListValue(List(IntegerValue(1))))
      result shouldBe Right(ListValue.nil)
    }

    it("should return ListValue.nil for greaterThan comparison of lists") {
      val result = ListRelational.greaterThan(ListValue.nil, ListValue.nil)
      result shouldBe Right(ListValue.nil)
    }

    it("should return ListValue.nil for lessThan comparison of lists") {
      val result = ListRelational.lessThan(ListValue.nil, ListValue.nil)
      result shouldBe Right(ListValue.nil)
    }
  }
}
