package lectures.functions
import org.scalatest.{FunSpec, Matchers}

class FunctionalComputationSpec extends FunSpec with Matchers {
  val filterData = "Клара у Карла украла корралы, Карл у Клары украл кларнет"
  val dataArray = "Клара Цеткин обожала Карла Маркса".split(" ")
  describe("Object FunctionalComputationSpec") {
    describe("result") {
      it("Should retrun Array(\"Клара\", \"Карла\")") {
        CurriedComputation.result should be (Array("Клара", "Карла"))
      }
    }
  }
}
