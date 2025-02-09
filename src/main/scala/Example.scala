import doodle.core.{Color, Point}
import doodle.java2d.{Algebra, Drawing, Frame}
import doodle.syntax._
import doodle.interact.syntax._
import doodle.java2d._

// Run this code to see an animation
object Example extends App {
  val frame = Frame.size(600, 600).background(Color.mistyRose)

  //val planet = circle[Algebra, Drawing](10).strokeColor(day1.Color.blueViolet)
  val planet = square[Algebra, Drawing](10).strokeColor(Color.blueViolet)

  val r = 250.0

  // y had 3 times the frequency of x. Combined they create an interesting
  // Lissajous figure.
  val x = (0.0).upTo(560.0).forSteps(560).map(a => Point(r, a.degrees))
  val y =
    (0.0).upTo(560.0).forSteps(520).map(a => Point(r, a.degrees)).repeat(3)

  val lissajous =
    x.product(y).map { case (x, y) => planet.at(x.x, y.y) }.repeat(3)
  lissajous.animate(frame)
}
