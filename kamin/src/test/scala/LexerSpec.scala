import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LexerSpec extends AnyFunSpec with Matchers {
  
  describe("tokens method") {
    it("should ignore whitespaces so '     ' should be empty") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("     ")
      it.hasNext shouldBe false
    }

    it("should treat single parts of text as a token") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("token")
      it.hasNext shouldBe true
      it.next().literal shouldBe "token"
      it.hasNext shouldBe false
    }

    it("should treat given separator char as a separator between tokens") {
      val lexer = Lexer(Seq(LeftParenthesisToken), Seq.empty)

      val it = lexer.tokens("token1(token2")
      it.hasNext shouldBe true
      it.next().literal shouldBe "token1"

      it.hasNext shouldBe true
      it.next().literal shouldBe "("

      it.hasNext shouldBe true
      it.next().literal shouldBe "token2"
      it.hasNext shouldBe false
    }
    
    it("should treat space as a separator between tokens") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("token5 token6")
      it.hasNext shouldBe true
      it.next().literal shouldBe "token5"

      it.hasNext shouldBe true
      it.next().literal shouldBe "token6"
      it.hasNext shouldBe false
    }

    it("should treat tab as a separator between tokens") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("443\t-5676")
      it.hasNext shouldBe true
      it.next().literal shouldBe "443"

      it.hasNext shouldBe true
      it.next().literal shouldBe "-5676"
      it.hasNext shouldBe false
    }

    it("should treat return as a separator between tokens") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("token9\rtoken0")
      it.hasNext shouldBe true
      it.next().literal shouldBe "token9"

      it.hasNext shouldBe true
      it.next().literal shouldBe "token0"
      it.hasNext shouldBe false
    }

    it("should treat newline as a separator between tokens") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("TokenA\nTokenB")
      it.hasNext shouldBe true
      it.next().literal shouldBe "TokenA"

      it.hasNext shouldBe true
      it.next().literal shouldBe "TokenB"
      it.hasNext shouldBe false
    }

    it("should match strings as given keywords with keywords") {
      val lexer1 = Lexer(Seq.empty, Seq.empty)
      val lexer2 = Lexer(Seq.empty, Seq(IfToken))

      val token1 = lexer1.tokens("if").next()
      val token2 = lexer2.tokens("if").next()

      token1 shouldBe NameToken("if")
      token2 shouldBe IfToken
    }
  }
}
