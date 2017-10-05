package lectures.functions
import org.scalatest.{FunSpec, Matchers}

class CurriedComputationSpec extends FunSpec with Matchers {
  val filterData = "Клара у Карла украла корралы, Карл у Клары украл кларнет"
  val dataArray = "Клара Цеткин обожала Карла Маркса".split(" ")
  describe("Object CurriedComputation") {
    describe("result") {
      it("Should retrun Array(\"Клара\", \"Карла\")") {
        CurriedComputation.result should be (Array("Клара", "Карла"))
      }
    }
  }
}