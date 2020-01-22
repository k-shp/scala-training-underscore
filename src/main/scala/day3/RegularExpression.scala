//sealed trait RegexComponent {
//  def matches(input: String): MatchResult
//  def andThen(other: RegexComponent): AndThen = AndThen(this, other)
//}
//
//final case class RegexChar(char: Char) extends RegexComponent {
//  def matches(input: String): MatchResult = {
//    if (input.startsWith(char.toString))
//      YesMatch(char.toString, input.tail)
//    else
//      NoMatch()
//  }
//}
//
//final case class AndThen(one: RegexComponent, two: RegexComponent)
//    extends RegexComponent {
//  def matches(input: String): MatchResult = {
//
//    val r1 = one.matches(input)
//    val r2 = r1 match {
//      case YesMatch(_, remaining) => two.matches(remaining)
//      case NoMatch()              => NoMatch()
//    }
//
//    (r1, r2) match {
//      case (YesMatch(m1, _), YesMatch(m2, r2)) => YesMatch(m1 + m2, r2)
//      case _                                   => NoMatch()
//    }
//  }
//}
//
//case class MyRegex(pattern: String) extends RegexComponent {
//  def matches(input: String): MatchResult = {
//    if (input.isEmpty) NoMatch()
//    else NoMatch() //TODO
////      input.reduceLeft((c1, c2) =>
////        RegexChar(c1).andThen(RegexChar(c2)).matches(input)
////      )
//  }
//}
//
//sealed trait MatchResult
//final case class YesMatch(matched: String, remaining: String)
//    extends MatchResult
//final case class NoMatch() extends MatchResult
//
//object MyRegexTest {
//  def main(args: Array[String]): Unit = {
////    println(RegexChar('a').andThen(RegexChar('b')) matches ("abs"))
////    println(RegexChar('a').andThen(RegexChar('b')) matches ("bb"))
//    // println(println(MyRegex("hi").matches("heyhi")))
//  }
//}
