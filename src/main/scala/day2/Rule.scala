package day2

case class Rule[A, B](f: A => Either[List[String], B]) {
  def apply(value: A): Either[List[String], B] = f(value)

  def map[C](f: B => C): Rule[A, C] = {
    Rule(a => this(a).map(f))
  }
}

object RuleTest {
  def main(args: Array[String]): Unit = {
    val r = Rule[Int, Double](x =>
      if (x > 0) Right(x * 2.0) else Left(List("negative number!"))
    ).map[Int](_.toInt)
  }
}
