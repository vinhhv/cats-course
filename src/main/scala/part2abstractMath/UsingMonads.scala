package part2abstractMath

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}

object UsingMonads {

  import cats.Monad
  import cats.instances.future.*
  import cats.instances.list.*
  import cats.instances.option.*
  val monadList      = Monad[List]       // fetch the implicit Monad[List]
  val aSimpleList    = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // either is also a Monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T]   = Either[Throwable, T]
  import cats.instances.either.*
  val loadingMonad = Monad[LoadingOr]
  val anEither     = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading =
    loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life..."))

  // imaginary online store
  final case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000)
      Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId       = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))

  // use extension methods
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  val orderLocationBetter = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))
  val orderLocationFor = for {
    orderStatus <- getOrderStatus(orderId)
    location    <- trackLocation(orderStatus)
  } yield location

  // TODO: the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }
  // DO NOT CHANGE THE CODE

  /*
    Requirements:
    - if the host and port in the configuration map, then return a M containing a connection with those values,
      otherwise the method will fail, according to the logic of type M
      (for Try it will return a Failure, for Option it will return a None, Future => Failure, Either => Left
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload
      is less than 20 characters, otherwise the method will fail, according to the logic of the type M
   */

  // TODO: provide a real implementation of HttpService using Try, Option, Future, Either

  // 2. TC-instances
  given optionHttpService: HttpService[Option] with {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) Some(s"Request $payload has been accepted")
      else None
  }

  val executor: ExecutorService = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext    = ExecutionContext.fromExecutor(executor)

  given futureHttpService: HttpService[Future] with {
    override def getConnection(cfg: Map[String, String]): Future[Connection] =
      for {
        host <- cfg.get("host") match
          case Some(value) => Future.successful(value)
          case None        => Future.failed(new NoSuchElementException(s"Host was not found in config $cfg"))
        port <- cfg.get("port") match
          case Some(value) => Future.successful(value)
          case None        => Future.failed(new NoSuchElementException(s"Port was not found in config $cfg"))
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Future[String] =
      if (payload.length < 20) Future.successful(s"request $payload has been accepted")
      else Future.failed(new RuntimeException(s"Payload $payload exceeds maximum 20 char count"))
  }

  // TODO: implement another HTTPService with LoadingOr or ErrorOr
  given errorOrHttpService: HttpService[ErrorOr] with {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      for {
        host <- cfg.get("host") match
          case Some(host) => Right(host)
          case None       => Left(new Throwable(s"Host does not exist in config $cfg"))
        port <- cfg.get("port") match
          case Some(port) => Right(port)
          case None       => Left(new Throwable(s"Port does not exist in config $cfg"))
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length < 20) Right(s"Request $payload has been accepted")
      else Left(new Throwable(s"Payload $payload exceeds maximum 20 char count"))
  }

  // 3. user-facing API
  object HttpService {
    def getConnectionM[M[_]](cfg: Map[String, String])(using httpService: HttpService[M]): M[Connection] =
      httpService.getConnection(cfg)

    def issueRequestM[M[_]](connection: Connection, payload: String)(using httpService: HttpService[M]): M[String] =
      httpService.issueRequest(connection, payload)

    def getResponse[M[_]: Monad](payload: String)(using service: HttpService[M]): M[String] =
      for {
        conn     <- service.getConnection(config)
        response <- service.issueRequest(conn, payload)
      } yield response
  }

  def main(args: Array[String]): Unit = {
    val optionConnection = for {
      connection <- HttpService.getConnectionM[Option](config)
      response   <- HttpService.issueRequestM[Option](connection, "hello option")
    } yield response
    println(optionConnection)

    val futureConnection = for {
      connection <- HttpService.getConnectionM[Future](config)
      response   <- HttpService.issueRequestM[Future](connection, "from future")
    } yield response

    val errorOrConnection = for {
      connection <- HttpService.getConnectionM[ErrorOr](config)
      response   <- HttpService.issueRequestM[ErrorOr](connection, "from ErrorOr")
    } yield response
    println(errorOrConnection)

    // println(HttpService.getResponse(optionHttpService, "response option"))
    println(HttpService.getResponse[Option]("response option"))

    Thread.sleep(1000)
    println(futureConnection.value)
    executor.shutdown()
  }
}
