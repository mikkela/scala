import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StringExtensionsSpec extends AnyFunSpec  with Matchers {
    describe("removeComment") {
        it("should remove text after the first semicolon") {
            "Hello; world!".removeComment shouldBe "Hello"
        }

        it("should return the whole string if no semicolon is present") {
            "No semicolon here".removeComment shouldBe "No semicolon here"
        }

        it("should return an empty string if the string starts with a semicolon") {
            ";Starts with semicolon".removeComment shouldBe ""
        }

        it("should stop at the first semicolon, ignoring any subsequent semicolons") {
            "First;Second;Third".removeComment shouldBe "First"
        }

        it("should handle an empty string gracefully") {
            "".removeComment shouldBe ""
        }
    }
}
