import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class BasicLexerSpec extends AnyFunSpec
  with Matchers
  with TableDrivenPropertyChecks {

  private val singleTokenTable = Table(
    ("Text", "Is Token"),
    ("(", Token(TokenType.LeftParenthesis, "(")),
    (")", Token(TokenType.RightParenthesis, ")")),
    ("=", Token(TokenType.Equal, "=")),
    ("+", Token(TokenType.Plus, "+")),
    ("-", Token(TokenType.Minus, "-")),
    ("*", Token(TokenType.Asterisk, "*")),
    ("/", Token(TokenType.Slash, "/")),
    ("<", Token(TokenType.LessThan, "<")),
    (">", Token(TokenType.GreaterThan, ">")),
    ("define", Token(TokenType.Define, "define")),
    ("print", Token(TokenType.Print, "print")),
    ("if", Token(TokenType.If, "if")),
    ("while", Token(TokenType.While, "while")),
    ("set", Token(TokenType.Set, "set")),
    ("begin", Token(TokenType.Begin, "begin")),
    ("abe", Token(TokenType.Name, "abe")),
    ("-345", Token(TokenType.Integer, "-345")),
    ("56", Token(TokenType.Integer, "56"))
  )

  describe("toToken method") {
      it("should convert the provided text correctly") {
        forAll(singleTokenTable) {
          (str, expectedToken) =>
            BasicLexer.tokens(str).next() shouldBe expectedToken
        }
      }
  }
}
