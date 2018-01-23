package lectures.oop

import java.security.MessageDigest


/**
  * Данный класс содержит код, наспех написанный одним джуниор-разработчиком,
  * который плохо слушал лекции по эффективному программированию.
  *
  * Вам необходимо:
  * - отрефакторить данный класс, выделив уровни ответственности, необходимые
  *   интерфейсы и абстракции
  * - дописать тесты в FatUglyControllerTest и реализовать в них проверку на
  *   сохранение в БД, отправку сообщения в очередь и отправку email-а
  * - исправить очевидные костыли в коде
  *
  * Код внутри методов, помеченный как DO NOT TOUCH, трогать нельзя (сами методы
  * при этом можно выносить куда и как угодно)
  *
  * Интерфейс метода processRoute менять можно и нужно!
  * Передаваемые данные при этом должны оставаться неизменными.
  *
  * Удачи!
  */

trait FatUglyController {
  def processRoute(route: String, requestBody: Option[Array[Byte]]): RouteResult
}

trait FatUglyControllerComponent {
  def fatUglyController: FatUglyController
}

class FatUglyControllerImpl extends FatUglyController {

  override def processRoute(route: String, requestBody: Option[Array[Byte]]): RouteResult = {
    val responseBuf = new StringBuilder()
    val databaseConnectionId = connectToPostgresDatabase()
    val mqConnectionId = connectToIbmMq()
    initializeLocalMailer()

    if (route != correctRoute) {
      RouteResult(404, "Route not found")
    } else {
      requestBody match {
        case None =>
          RouteResult(400, "Can not upload empty file")

        case Some(requestBytes) if requestBytes.length > maxRequestLength =>
          RouteResult(400, "File size should not be more than 8 MB")

        case Some(requestBytes) =>
          val stringBody = getStringBody(requestBytes)
          val delimiter = getDelimiter(stringBody)
          val trimedFiles = trimFiles(stringBody, delimiter)

          if (!isAllFilesValid(trimedFiles)) {
            RouteResult(400, "Request contains forbidden extension")
          } else {
            trimedFiles.foreach { trimedFile =>
              val (trimedName, body) = trimedFile.splitAt(trimedFile.indexOf('\n'))
              val trimmedBody = body.trim
              val extension = getExtention(trimedFile)
              val id = hash(trimedFile)
              
              // Emulate file saving to disk
              responseBuf.append(s"- saved file $trimedName to " + id + "." + extension + s" (file size: ${trimmedBody.length})\r\n")

              executePostgresQuery(databaseConnectionId, s"insert into files (id, name, created_on) values ('$id', '$trimedName', current_timestamp)")
              sendMessageToIbmMq(mqConnectionId, s"""<Event name="FileUpload"><Origin>SCALA_FTK_TASK</Origin><FileName>$trimedName</FileName></Event>""")
              send("admin@admin.tinkoff.ru", "File has been uploaded", s"Hey, we have got new file: $trimedName")

            }
            RouteResult(200, "Response:\r\n" + responseBuf.dropRight(2))
          }
      }
    }
  }
  
  private def getStringBody(requestBytes: Array[Byte]): String = {
    requestBytes.
      map(_.toChar).
      mkString.
      replaceAll("\r", "")
  }
  
  private def getDelimiter(stringBody: String): String = {
    stringBody.
      split("\n").
      head
  }
  
  private def trimFiles(stringBody: String, delimiter: String): Array[String] = {
    stringBody.
      split(delimiter).
      tail.
      map(_.trim)
  }
  
  private def getExtention(trimedFile: String): String = {
    val trimedName = trimedFile.splitAt(trimedFile.indexOf('\n'))._1
    trimedName.drop(trimedName.lastIndexOf('.') + 1)
  }
  
  private def isAllFilesValid(trimedFiles: Array[String]): Boolean = {
    trimedFiles.forall { trimedFile =>
      val extention = getExtention(trimedFile)
      !notValidExtensions.contains(extention)
    }
  }
  
  private def connectToPostgresDatabase(): Int = {
    // DO NOT TOUCH
    println("Connected to PostgerSQL database")
    42 // pretty unique connection id
  }

  private def executePostgresQuery(connectionId: Int, sql: String): String = {
    // DO NOT TOUCH
    println(s"Executed SQL statement on connection $connectionId: $sql")
    s"Result of $sql"
  }

  private def connectToIbmMq(): Int = {
    // DO NOT TOUCH
    println("Connected to IBM WebSphere super-duper MQ Manager")
    13 // chosen by fair dice roll
  }

  private def sendMessageToIbmMq(connectionId: Int, message: String): String = {
    // DO NOT TOUCH
    println(s"Sent MQ message via $connectionId: $message")
    s"Message sending result for $message"
  }

  private def initializeLocalMailer(): Unit = {
    // DO NOT TOUCH
    println("Initialized local mailer")
  }

  private def send(email: String, subject: String, body: String): Unit = {
    // DO NOT TOUCH
    println(s"Sent email to $email with subject '$subject'")
  }

  private def hash(s: String): String = {
    MessageDigest.getInstance("SHA-1").digest(s.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

  private val correctRoute: String = "/api/v1/uploadFile"
  private val maxRequestLength: Int = 8388608
  private val notValidExtensions = Seq("exe", "bat", "com", "sh")

}

case class RouteResult(code: Int, message: String)