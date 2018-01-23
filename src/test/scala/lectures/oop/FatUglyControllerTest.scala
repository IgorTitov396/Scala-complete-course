package lectures.oop

import org.scalatest.{FlatSpec, Matchers}

class FatUglyControllerTest extends FlatSpec with Matchers {

  behavior of "FatUglyController"

  it should "successfully process single file" in {
    val requestBody =
      """DELIMITER
        |file1.txt
        |This is body of file1
      """.stripMargin
    val RouteResult(status, body) = controller.processRoute("/api/v1/uploadFile", Some(requestBody.getBytes))

    status shouldBe 200
    body shouldBe
      """Response:
        |- saved file file1.txt to 063f83f94e59aac2edd719fab1d179f86084887a.txt (file size: 21)"""
        .stripMargin
  }

  it should "successfully process two files" in {
    val requestBody =
      """DELIMITER22
        |file1.txt
        |This is body of file1
        |DELIMITER22
        |file2.txt
        |This is body of file2!!
      """.stripMargin
    val RouteResult(status, body) = controller.processRoute("/api/v1/uploadFile", Some(requestBody.getBytes))

    status shouldBe 200
    body shouldBe
      """Response:
        |- saved file file1.txt to 063f83f94e59aac2edd719fab1d179f86084887a.txt (file size: 21)
        |- saved file file2.txt to 7387fa41a69d93b59b67bd46ab18a72c81edb767.txt (file size: 23)"""
        .stripMargin
  }

  it should "return 404 for unknown route" in {
    val RouteResult(status, body) = controller.processRoute("/api", None)

    status shouldBe 404
    body shouldBe "Route not found"
  }

  it should "return 400 for empty body" in {
    val RouteResult(status, body) = controller.processRoute("/api/v1/uploadFile", None)

    status shouldBe 400
    body shouldBe "Can not upload empty file"
  }

  it should "return 400 for forbidden extension" in {
    val requestBody =
      """DELIMITER
        |file1.exe
        |This is body of file1
      """.stripMargin
    val RouteResult(status, body) = controller.processRoute("/api/v1/uploadFile", Some(requestBody.getBytes))

    status shouldBe 400
    body shouldBe "Request contains forbidden extension"
  }

  it should "return 400 for file greater than 8 MB" in {
    val bigRequestLength: Int = 8388609
    val bigRequestBody: Array[Byte] = Array.fill[Byte](bigRequestLength)(0)

    val RouteResult(status, body) = controller.processRoute("/api/v1/uploadFile", Some(bigRequestBody))

    status shouldBe 400
    body shouldBe "File size should not be more than 8 MB"
  }

  private val controller: FatUglyController = new FatUglyControllerImpl()

}
