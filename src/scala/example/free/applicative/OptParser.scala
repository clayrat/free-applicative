package example.free.applicative

import free.applicative.FreeA

import scalaz._
import Scalaz._

case class OptParser[+A](optName: String, optDefault: Option[A], optReader: String => Option[A])

object OptParser {

  import FreeA._

  implicit val OptParserFunctor: Functor[OptParser] = new Functor[OptParser] {
    def map[A, B](op: OptParser[A])(f: A => B): OptParser[B] =
      OptParser(op.optName,
        op.optDefault map f,
        op.optReader andThen (_ map f))
  }

  def parserDefault[A](fa: FreeA[OptParser, A]): Option[A] = fa.run { x: OptParser[A] => x.optDefault}


  type CL[A] = Const[List[String], A]

  def allOptions[A](fa: FreeA[OptParser, A]): List[String] = {
    /*  this fails with "could not find implicit value for evidence parameter of type scalaz.Applicative[G]" for Const
        I suspect it's due to Scala's inability to infer type lambdas
     */
    //    fa.run { x: OptParser[X] => Const[List[String], X](List(x.optName))}
    //      .getConst

    def f[X](opt: OptParser[X]) = Const[List[String], X](List(opt.optName))
    def helper[X](fa: FreeA[OptParser, X]): Const[List[String], X] = fa match {
      case Pure(x) => Const[List[String], X](Nil)
      case Ap(x: OptParser[X], g) =>
        val hg: Const[List[String], X => X] = helper(g)
        f(x) <*> hg
    }
    helper(fa).getConst
  }

  def matchOpt[A](opt: String, value: String, fa: FreeA[OptParser, A]): Option[FreeA[OptParser, A]] = fa match {
    case Pure(_) => None
    case Ap(x: OptParser[A], g: FreeA[OptParser, Any => A]) if opt == s"--${x.optName}" =>
      x.optReader(value) map { y: A => g map { f: (Any => A) => f(y)}}
    case Ap(x, g) => matchOpt(opt, value, g) map (Ap(x, _))
  }

  def runParser[A](fa: FreeA[OptParser, A], ops: List[String]): Option[A] = ops match {
    case opt :: value :: args => matchOpt(opt, value, fa) match {
      case None => None
      case Some(fp) => runParser(fp, args)
    }
    case Nil => parserDefault(fa)
    case _ => None
  }

  def main(args: Array[String]) {

    case class User(username: String, fullname: String, id: Int)

    def readInt(str: String): Option[Int] = {
      import scala.util.control.Exception._
      catching(classOf[NumberFormatException]) opt str.toInt
    }

    val userP: FreeA[OptParser, User] =
      liftFreeA(OptParser("id", None, readInt)) <*> (
        liftFreeA(OptParser("fullname", Some(""), Some(_))) <*> (
          liftFreeA(OptParser("username", None, Some(_))) map User.curried
          )
        )

    val parsed = runParser(userP, List(
      "--id", "1",
      "--fullname", "Adam",
      "--username", "mada")
    )

    println(parsed)


  }

}

