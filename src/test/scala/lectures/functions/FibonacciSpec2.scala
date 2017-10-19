package lectures.functions
import org.scalatest.{FunSpec, Matchers}

class FibonacciSpec2 extends FunSpec with Matchers {
  describe("FibonacciTest2") {
    describe("Method fibs2") {
      it("Should return 28657") {
        Fibonacci2.fibs2(24) should be (46368)
      }
      it ("Shuld retrun 0") {
        Fibonacci2.fibs2(0) should be (0)
      }
      it ("Should throw RuntimeException") {
        an [RuntimeException] should be thrownBy (Fibonacci2.fibs2(-4))
      }
    }
  }
}
