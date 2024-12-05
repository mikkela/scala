package kamin.apl

import kamin.{FunctionCallExpressionNode, FunctionDefinitionNode, IntegerValue, IntegerValueExpressionNode, PeekingIterator, VariableExpressionNode}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class APLSpec extends AnyFunSpec
  with Matchers {

  describe("Parsing and evaluating of Factorial") {
    it("should parse the definition correctly") {
      APLParser.parse(PeekingIterator(APLLexer.tokens("(define fac (n) (*/ (indx n)))")))(using APLParserContext) shouldBe
        Right(FunctionDefinitionNode("fac", Seq("n"), MultiplicationReductionExpressionNode(IndexGenerationExpressionNode(VariableExpressionNode("n")))))
    }
    
    it("should parse the usage correctly") {
      APLParser.parse(PeekingIterator(APLLexer.tokens("(fac 5)")))(using APLParserContext) shouldBe
        Right(FunctionCallExpressionNode("fac", Seq(IntegerValueExpressionNode(IntegerValue(5)))))
    }
  }
  
  

}
