package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {
  // provides map and flatMap for nested monads, so you don't have to unwrap your monads when mapping/flatMapping

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // option transformer
  import cats.data.OptionT
  import cats.instances.list.* // fetch an implicit Monad[List]

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char]  = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char   <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // either transformer
  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))

  val es                                           = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext                       = ExecutionContext.fromExecutorService(es)
  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future(Right(45).withLeft[String]))

  // TODO exercise
  /*
    We have a multi-machine cluster for your business which will receive a traffic surge following something.
    We measure bandwidth in units.
    We want to allocate TWO of our servers to cope with the traffic spike.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
   */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None    => EitherT.left(Future(s"Server $server unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }

  // TODO 1
  // hint: call getBandwidth twice, and combine the results
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    bw1 <- getBandwidth(s1)
    bw2 <- getBandwidth(s2)
  } yield bw1 + bw2 > 250

  // TODO 2
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(message) => Left(s"Cannot withstand: $message")
      case Right(true)   => Right("Can withstand bandwidth")
      case Right(false)  => Left("Cannot withstand bandwidth")
    }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)

    val trafficSpikeReport1 = generateTrafficSpikeReport("server1.rockthejvm.com", "server2.rockthejvm.com").value
    val trafficSpikeReport2 = generateTrafficSpikeReport("server1.rockthejvm.com", "server3.rockthejvm.com").value
    val trafficSpikeReport3 = generateTrafficSpikeReport("server4.rockthejvm.com", "server3.rockthejvm.com").value
    Thread.sleep(1000)
    println(trafficSpikeReport1)
    println(trafficSpikeReport2)
    println(trafficSpikeReport3)

    es.shutdown()
  }
}
