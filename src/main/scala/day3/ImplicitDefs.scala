case class Cat(age: Int)

object Cat {
  implicit val catOrdering: Ordering[Cat] = {
    new Ordering[Cat] {
      def compare(x: Cat, y: Cat): Int = {
        x.age.compareTo(y.age)
      }
    }
  }
}

case class Box[A](value: A)

object Box {
  implicit def boxOrdering[A](
      implicit aOrdering: Ordering[A]
  ): Ordering[Box[A]] =
    new Ordering[Box[A]] {
      def compare(x: Box[A], y: Box[A]): Int = {
        aOrdering.compare(x.value, y.value)
      }
    }
}

object BoxTest extends App {
  val boxes = List(Box[Cat](Cat(1)), Box[Cat](Cat(4)), Box[Cat](Cat(2)))
  val sorted = boxes.sorted(Box.boxOrdering[Cat](Cat.catOrdering))
  println(sorted)
}
