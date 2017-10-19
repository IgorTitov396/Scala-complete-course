package lectures.functions
import org.scalatest.{FunSpec, Matchers}

class FibonacciSpec extends FunSpec with Matchers {
  describe("FibonacciTest") {
    describe("Method fibs") {
      it("Should return 28657") {
        Fibonacci.fibs(24) should be (46368)
      }
      it ("Shuld retrun 0") {
        Fibonacci.fibs(0) should be (0)
      }
      it ("Should throw RuntimeException") {
        an [RuntimeException] should be thrownBy (Fibonacci.fibs(-4))
      }
    }
  }
}
