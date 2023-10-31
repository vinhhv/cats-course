package part3datamanipulation

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer
  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)

  // 2 - manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1)                                     // value increases, logs stay the same
  val aLogsWriter       = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth   = aWriter.bimap(_ :+ "found something interesting", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }

  // flatMap
  import cats.instances.vector.* // imports a Semigroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list.* // an implicit Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset // clears the logs, keep the value

  // 3 - dump either the value or the logs
  val desiredValue = aWriter.value
  val logs         = aWriter.written
  val (l, v)       = aWriter.run

  // TODO 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  // def countAndLog(n: Int): Writer[Vector[String], Int] = {
  //  val writer = Writer(Vector("starting!"), 0)

  //  def tailRec(count: Int, writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
  //    val w = if (count > 1) tailRec(count - 1, writer) else writer
  //    w.bimap(_ :+ count.toString, _ + 1)
  //  }
  //  tailRec(n, writer)
  // }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), 0)
    else countAndLog(n - 1).bimap(_ :+ n.toString, _ => n)
  }

  // Benefit #1: we work with pure FP

  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}} = $lowerSum")
      lowerSum + n
    }
  }

  def betterSum(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector[String](), 0)
    else {
      val now      = s"Now at $n"
      val computed = (lowerSum: Int) => s"Computed sum(${n - 1}} = $lowerSum"
      betterSum(n - 1).mapBoth { (logs, lowerSum) =>
        (now +: (logs :+ computed(lowerSum)), lowerSum + n)
      }
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else
      for {
        _        <- Writer.tell(Vector(s"Now at $n"))
        lowerSum <- sumWithLogs(n - 1)
        _        <- Writer.tell(Vector(s"Computed sum ${n - 1}"))
      } yield lowerSum + n
  }

  // Benefit #2: Writers can keep logs separate on multiple threads

  given ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    println(countAndLog(10).written.foreach(println))
    println(countAndLog(10).run)

    // println(naiveSum(10))
    val better = betterSum(10)
    better.written.foreach(println)
    println(better.value)

    // ex 2
    val sum = sumWithLogs(10)
    sum.written.foreach(println)
    println(sum.value)

    val sumFuture1 = Future(sumWithLogs(10))
    val sumFuture2 = Future(sumWithLogs(10))
    val logs1      = sumFuture1.map(_.written) // logs from thread1
    val logs2      = sumFuture2.map(_.written) // logs from thread2
  }
}
