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

//components

trait FatUglyControllerComponent {
  def fatUglyController: FatUglyController
}

trait MqServiceComponent {
  def mqService: MqService
}

trait DbServiceComponent {
  def dbService: DbService
}

trait MailerComponent {
  def mailer: Mailer
}

trait RequestValidationServiceComponent {
  def requestValidationService: RequestValidationService
}

//services

trait FatUglyController {
  def processRoute(route: String, requestBody: Option[Array[Byte]]): RouteResult
}

trait DbService {
  def connectAndGetId: Int
  def execute(connectionId: Int, query: String): String
}

trait MqService {
  def connectAndGetId: Int
  def sendMessage(connectionId: Int, message: String): String
}

trait Mailer {
  def init(): Unit

  def sendToEmail(email: String, subject: String, body: String): Unit
}

trait RequestValidationService {
  def extractRequestBody(request: Request): Either[RouteResult, Array[Byte]]
}

//implementations

class PostgresDbServiceImpl extends DbService {

  override def connectAndGetId: Int = connectToPostgresDatabase()

  override def execute(connectionId: Int, query: String): String =
    executePostgresQuery(connectionId, query)


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

}

class IbmMqServiceImpl extends MqService {

  override def connectAndGetId: Int = connectToIbmMq()

  override def sendMessage(connectionId: Int, message: String): String =
    sendMessageToIbmMq(connectionId, message)

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

}

class LocalMailerImpl extends Mailer {

  override def init(): Unit = initializeLocalMailer()

  override def sendToEmail(email: String, subject: String, body: String): Unit =
    send(email: String, subject: String, body: String)

  private def initializeLocalMailer(): Unit = {
    // DO NOT TOUCH
    println("Initialized local mailer")
  }

  private def send(email: String, subject: String, body: String): Unit = {
    // DO NOT TOUCH
    println(s"Sent email to $email with subject '$subject'")
  }

}

class RequestValidationServiceImpl extends RequestValidationService {
  override def extractRequestBody(request: Request): Either[RouteResult, Array[Byte]] = {
    if (!isRouteCorrect(request.route)) Left(RouteResult(404, "Route not found"))
    else {
      request.requestBodyOpt match {
        case None =>
          Left(RouteResult(400, "Can not upload empty file"))

        case Some(requestBytes) if requestBytes.length > maxRequestLength =>
          Left(RouteResult(400, "File size should not be more than 8 MB"))

        case Some(requestBody) => Right(requestBody)
      }
    }
  }

  private def isRouteCorrect(route: String) = route.equals(correctRoute)

  private val maxRequestLength: Int = 8388608

  private val correctRoute: String = "/api/v1/uploadFile"

  case class ValidRequest()
}

object RequestParserUtil {

  def getTrimedFilesFromRequestBody(requestBody: Array[Byte]): Array[String] = {
    val stringBody = getStringBody(requestBody)
    val delimiter = getDelimiter(stringBody)
    trimFiles(stringBody, delimiter)
  }

  private def getStringBody(requestBytes: Array[Byte]): String = {
    requestBytes.
      map(_.toChar)
      .mkString
      .replaceAll("\r", "")
  }

  private def getDelimiter(stringBody: String): String = {
    stringBody
      .split("\n")
      .head
  }

  private def trimFiles(stringBody: String, delimiter: String): Array[String] = {
    stringBody
      .split(delimiter)
      .tail
      .map(_.trim)
  }
}


class FatUglyControllerImpl extends FatUglyController {
  this: DbServiceComponent
    with MqServiceComponent
    with MailerComponent
    with RequestValidationServiceComponent =>

  override def processRoute(route: String, requestBodyOpt: Option[Array[Byte]]): RouteResult = {
    requestValidationService.extractRequestBody(Request(route, requestBodyOpt)) match {
      case Left(routeResult) => routeResult
      case Right(requestBody) =>
        val trimedFiles = RequestParserUtil.getTrimedFilesFromRequestBody(requestBody)
        storeAndSpreadFiles(trimedFiles)
    }
  }

  private def storeAndSpreadFiles(files: Seq[String]): RouteResult = {
    val databaseConnectionId = dbService.connectAndGetId
    val mqConnectionId = mqService.connectAndGetId
    mailer.init()

    if (!isAllFilesValid(files)) {
      RouteResult(400, "Request contains forbidden extension")
    } else {
      val responseBuf = new StringBuilder()

      files.foreach { trimedFile =>
        val (trimedName, body) = trimedFile.splitAt(trimedFile.indexOf('\n'))
        val fileSize = body.trim.length
        val extension = getExtention(trimedFile)
        val id = hash(trimedFile)

        // Emulate file saving to disk
        responseBuf.append(s"- saved file $trimedName to " + id + "." + extension + s" (file size: $fileSize)\r\n")

        dbService.execute(databaseConnectionId, s"insert into files (id, name, created_on) values ('$id', '$trimedName', current_timestamp)")
        mqService.sendMessage(mqConnectionId, s"""<Event name="FileUpload"><Origin>SCALA_FTK_TASK</Origin><FileName>$trimedName</FileName></Event>""")
        mailer.sendToEmail("admin@admin.tinkoff.ru", "File has been uploaded", s"Hey, we have got new file: $trimedName")

      }
      RouteResult(200, "Response:\r\n" + responseBuf.dropRight(2))
    }
  }



  private def getExtention(trimedFile: String): String = {
    val trimedName = trimedFile.splitAt(trimedFile.indexOf('\n'))._1
    trimedName.drop(trimedName.lastIndexOf('.') + 1)
  }

  private def isAllFilesValid(files: Seq[String]): Boolean = {
    files.forall { trimedFile =>
      val extention = getExtention(trimedFile)
      !notValidExtensions.contains(extention)
    }
  }

  private def hash(s: String): String = {
    MessageDigest.getInstance("SHA-1").digest(s.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

  private val notValidExtensions = Seq("exe", "bat", "com", "sh")

}

case class RouteResult(code: Int, message: String)

case class Request(route: String, requestBodyOpt: Option[Array[Byte]])