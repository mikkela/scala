import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class BasicLexerSpec extends AnyFunSpec
  with Matchers
  with TableDrivenPropertyChecks {

  private val singleTokenTable = Table(
    ("Text", "Is Token"),
    ("(", LeftParenthesisToken),
    (")", RightParenthesisToken),
    ("=", EqualToken),
    ("+", PlusToken),
    ("-", MinusToken),
    ("*", AsteriskToken),
    ("/", SlashToken),
    ("<", LessThanToken),
    (">", GreaterThanToken),
    ("define", DefineToken),
    ("print", PrintToken),
    ("if", IfToken),
    ("while", WhileToken),
    ("set", SetToken),
    ("begin", BeginToken),
    ("abe", NameToken("abe")),
    ("-345", IntegerValueToken("-345")),
    ("56", IntegerValueToken("56"))
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
