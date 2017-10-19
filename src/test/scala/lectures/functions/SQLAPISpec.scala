package lectures.functions
import org.scalatest.{FunSpec, Matchers}

class SQLAPISpec extends FunSpec with Matchers {
  describe("Class SQLAPISpec") {
    describe("execute function") {
      it("Should retrun SQL has been executed. Congrats!") {
        new SQLAPI("some DB").execute("some SQL") should be ("SQL has been executed. Congrats!")
      }
    }
  }
}
