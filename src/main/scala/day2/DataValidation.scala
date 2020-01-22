package day2

import scala.util.{Failure, Success, Try}

object DataValidation {
  def main(args: Array[String]): Unit = {
    println(isNonNegativeInt("10"))
    println(isNonNegativeInt("-5"))
    println(isNonNegativeInt("bla"))

    println(isNonNegativeIntFor("10"))
    println(isNonNegativeIntFor("-5"))
    println(isNonNegativeIntFor("bla"))

    println(isAlphaNumeric("lol"))
    println(isAlphaNumeric(":-)"))
    println(isAlphaNumeric(""))
    println(isNegative(4))
    println(isNegative(-4))
  }

  def isNegative(value: Int): Either[String, Int] = {
    predicate[Int](value, _ < 0, "number is not negative!")
  }

  def predicate[A](
      value: A,
      check: A => Boolean,
      error: String = "fail!"
  ): Either[String, A] = {
    if (check(value)) Right(value) else Left(error)
  }

  def parseInt(string: String): Either[List[String], Int] = {
    Try(string.toInt) match {
      case Success(value) => Right(value)
      case Failure(e)     => Left(List(e.toString))
    }
  }

  def isPositive(int: Int): Either[List[String], Int] = {
    if (int >= 0) Right(int) else Left(List("Number is not positive!"))
  }

  def isNonNegativeInt(string: String): Either[List[String], Int] = {
    parseInt(string).flatMap(isPositive)
  }

  def isNonNegativeIntFor(string: String): Either[List[String], Int] = {
    for {
      p <- parseInt(string)
      i <- isPositive(p)
    } yield i
  }

  def isNotEmpty(string: String): Option[String] = {
    if (string.nonEmpty) Some(string) else None
  }

  def isAlphaNumeric(string: String): Option[String] = {
    val nonempty = isNotEmpty(string)

    nonempty.flatMap(s =>
      if (s.forall(_.isLetterOrDigit)) Some(string) else None
    )
  }

}
