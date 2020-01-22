package day2

case class Predicate[A](f: A => Either[List[String], A]) {
  def apply(value: A): Either[List[String], A] = f(value)

  def and(that: Predicate[A]): Predicate[A] = {
    Predicate { a =>
      val res1 = this(a)
      val res2 = that(a)
      (res1, res2) match {
        case (Right(_), Right(_)) => Right(a)
        case (Right(_), Left(e))  => Left(e)
        case (Left(e), Right(_))  => Left(e)
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      }
    }
  }

  def or(that: Predicate[A]): Predicate[A] = {
    Predicate { a =>
      val res1 = this(a)
      val res2 = that(a)
      (res1, res2) match {
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case _                    => Right(a)
      }
    }
  }
}

object PredicateTest {
  def main(args: Array[String]): Unit = {
    val p1 = Predicate[Int](x =>
      if (x > 0) Right(x) else Left(List(s"$x must be above zero!"))
    )
    val p2 = Predicate[Int](x =>
      if (x > 10) Right(x) else Left(List(s"$x must be above ten!"))
    )
    val p3 = Predicate[Int](x =>
      if (x > 20) Right(x) else Left(List(s"$x must be above twenty!"))
    )

    println(p1(-1))
    println(p2(-1))
    println(p1.and(p2).and(p3)(-1))
    println(p1.or(p2).or(p3)(5))

  }
}
