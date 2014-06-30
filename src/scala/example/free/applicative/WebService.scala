package example.free.applicative

import java.net.URL

import free.applicative.FreeA

import scalaz._

sealed trait WebService[+A]

case class GET[+A](url: URL, params: List[String], result: String => A) extends WebService[A]

case class POST[+A](url: URL, params: List[String], body: String, cont: A) extends WebService[A]

object WebServiceTest {

  import FreeA._

  implicit def WebServiceFunctor: Functor[WebService] = new Functor[WebService] {
    def map[A, B](ws: WebService[A])(f: A => B): WebService[B] = ws match {
      case GET(url, params, result) => GET(url, params, result andThen f)
      case POST(url, params, body, cont) => POST(url, params, body, f(cont))
    }
  }

  def count[A](fa: FreeA[WebService, A]): Int = fa match {
    case Pure(x) => 0
    case Ap(_, g) => 1 + count(g)
  }

  def main(args: Array[String]) {

    val testURL = new URL("http://google.com")

    def sum3(a: Option[Int], b: Option[Int], c: Option[Int]) = (a, b, c) match {
      case (Some(ai), Some(bi), Some(ci)) => ai + bi + ci
      case _ => 0
    }
    
    val requests =
      liftFreeA[WebService, Option[Int]](GET(testURL, List("a"), { s => Some(1)})) <*> (
        liftFreeA[WebService, Option[Int]](POST(testURL, List("b"), "test", Some(2))) <*> (
          liftFreeA[WebService, Option[Int]](GET(testURL, List("c"), { s => Some(3)})) map (sum3 _).curried
          )
        )

    println(count(requests))

  }
}

