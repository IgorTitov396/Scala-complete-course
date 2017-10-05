package lectures.operators

import org.scalatest.{FunSpec, Matchers}

class CompetitionSpec extends FunSpec with Matchers {
  describe("Object Competition") {
    describe("results") {
      it("Should be right") {
        Competition.results.toMap should be (Map("Alexander vs John" -> 1, "Alexander vs James" -> 3, "Alexander vs Tom" -> 2, "Alexander vs Dick" -> -1, "Alexander vs Eric" -> -2, "Sergey vs Tom" -> 3, "Sergey vs Eric" -> -1, "Sergey vs Dick" -> 0, "Sergey vs John" -> 2, "Sergey vs James" -> 4, "Artem vs Eric" -> 0, "Artem vs John" -> 3, "Artem vs Tom" -> 4, "Artem vs Dick" -> 1, "Artem vs James" -> 5, "Anton vs John" -> -1, "Anton vs Tom" -> 0, "Anton vs Eric" -> -4, "Anton vs Dick" -> -3, "Anton vs James" -> 1, "Vladimir vs James" -> 1, "Vladimir vs Tom" -> 0, "Vladimir vs John" -> -1, "Vladimir vs Dick" -> -3, "Vladimir vs Eric" -> -4))
      }
    }
  }
}
