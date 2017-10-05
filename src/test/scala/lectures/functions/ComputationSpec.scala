package lectures.functions
import org.scalatest.{FunSpec, Matchers}

class ComputationSpec extends FunSpec with Matchers {
  val filterData = "Клара у Карла украла корралы, Карл у Клары украл кларнет"
  val dataArray = "Клара Цеткин обожала Карла Маркса".split(" ")
  describe("Object Computation") {
    describe("Method computation") {
      it("Shuld retrun Array(\"Клара\", \"Карла\")") {
        Computation.computation(filterData, dataArray) should be (Array("Клара", "Карла"))
      }
    }
  }
}
