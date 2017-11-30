import scala.collection.AbstractSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class MyFutures {

}

case class Url(name: String, urls: Seq[Url])

object Example1 extends App {


  val numF = Future {
    3
  }

  val stringF: Future[Future[String]] = numF.map(n => Future(n.toString))

  val flatStringF = numF.flatMap(n => Future(n.toString))


}

object Example2 extends App {

  val threeF = Future(3)
  val fourF = Future(4)
  val fiveF = Future(5)

  val resultF = for {
    three <- threeF
    four <- fourF
    five <- fiveF
  } yield {
    three * four * five
  }

  //  Await.result(resultF, 3 seconds)

  resultF.onComplete {
    case Success(result) => println(s"result = $result")
    case Failure(e) => e.printStackTrace
  }

}


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Prog {
  def main(args: Array[String]): Unit = {
    println("Starting Main")
    val tasks= Future.traverse(1 to 5)(myUrl)
    println("Continuing Main")
    // waits for all tasks to complete before exiting
    Await.result(tasks, Duration.Inf)
  }

  def myUrl(number: Int): Future[Url] = Future {
    println(s"Starting task#$number")
    val url = Url(s"google-$number", List(Url("link1", List()), Url("link2", List())))
    println(s"Finished task#$number")
    url
  }

  def startTask(number: Int): Future[Unit] = Future {
    println(s"Starting task#$number")
    Thread.sleep(2000) // wait 2secs
    println(s"Finished task#$number")
  }

  // ...
}



