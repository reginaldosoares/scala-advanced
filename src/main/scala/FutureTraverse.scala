import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global



object FutureTraverse extends App{
  private def log(s: String) = println(s"${Thread.currentThread.getName}: $s")

  def withDelay(i: Int) = Future{
    log(s"withDelay($i)")
    Thread.sleep(1000)
    i
  }



  Future {
    for(_ <- 0 to 5){
      log(".")
      Thread.sleep(1000)
    }
  }


  val resultSeq = Future.traverse(0 to 10)(withDelay)

  Thread.sleep(6000)
}