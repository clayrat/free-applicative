package example.free.applicative

import free.applicative.FreeA

import scalaz._
import Scalaz._

import scalaz.Const._

case class OptParser[+A](optName: String, optDefault: Option[A], optReader: String => Option[A]) {

}

object OptParser {

  import FreeA._

  implicit val OptParserFunctor: Functor[OptParser] = new Functor[OptParser] {
    def map[A, B](op: OptParser[A])(f: A => B): OptParser[B] =
      OptParser(op.optName,
        op.optDefault map f,
        op.optReader andThen (_ map f))
  }

  def parserDefault[A](fa: FreeA[OptParser, A]): Option[A] = fa.run { x: OptParser[A] => x.optDefault}

  def allOptions[A](fa: FreeA[OptParser, A]): List[String] = {
//    fa.run { x: OptParser[A] => Const[List[String], A](List(x.optName))}
//      .getConst

      def f[Y](opt: OptParser[Y]) = Const[List[String],Y](List(opt.optName))
      def helper[X](fa: FreeA[OptParser, X]): Const[List[String], X] = fa match {
        case Pure(x) => Const[List[String],X](Nil)
        case Ap(x: OptParser[X], g) =>
          val hg: Const[List[String],X => X] = helper(g)
          f(x) <*> hg
      }
      helper(fa).getConst
    }

  //  def matchOpt[A, B](opt: String, value: String, fa: FreeA[OptParser, A]): Option[FreeA[OptParser, A]] = fa match {
  //    case Pure(_) => None
  //    case Ap(x, g: FreeA[OptParser, Any => A]) if opt == s"--${x.optName}" =>
  //      val pure = x.optReader(value)
  //      pure map
  //    case Ap(x, g: FreeA[OptParser, A => B]) => matchOpt(opt, value, g)
  //  }

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

    println(allOptions(userP))

  }

}

