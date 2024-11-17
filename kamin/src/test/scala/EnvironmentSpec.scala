import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EnvironmentSpec extends AnyFunSpec with Matchers {

  describe("GlobalAndLocalScopeEnvironment") {

    it("should set and get a variable in the global scope") {
      val env = GlobalAndLocalScopeEnvironment()
      env.set("x", 42)
      env.get("x") shouldEqual Some(42)
    }

    it("should return None for an undefined variable") {
      val env = GlobalAndLocalScopeEnvironment()
      env.get("y") shouldEqual None
    }

    it("should open a new local scope and get variables from it") {
      val env = GlobalAndLocalScopeEnvironment()
      env.openScope(Seq("a", "b"))
      env.get("a") shouldEqual None
      env.set("a", 10)
      env.get("a") shouldEqual Some(10)
    }

    it("should shadow a global variable with a local variable") {
      val env = GlobalAndLocalScopeEnvironment()
      env.set("x", 42)
      env.openScope(Seq("x"))
      env.set("x", 100)
      env.get("x") shouldEqual Some(100)
    }

    it("should fall back to the global variable when no local variable is present") {
      val env = GlobalAndLocalScopeEnvironment()
      env.set("x", 42)
      env.openScope(Seq("y"))
      env.set("x", 44)
      env.get("x") shouldEqual Some(44)
      env.closeScope()
      env.get("x") shouldEqual Some(44)
    }
    

    it("should close the local scope and return to the global scope value") {
      val env = GlobalAndLocalScopeEnvironment()
      env.set("x", 42)
      env.openScope(Seq("x"))
      env.set("x", 100)
      env.get("x") shouldEqual Some(100)
      env.closeScope()
      env.get("x") shouldEqual Some(42)
    }

    it("should throw an exception if trying to close the global scope") {
      val env = GlobalAndLocalScopeEnvironment()
      an[IllegalStateException] should be thrownBy env.closeScope()
    }
  }
}