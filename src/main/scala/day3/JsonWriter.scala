trait Serialize[A] {
  def write(input: A): String
}

object Serialize {
  implicit val serializeInt = new Serialize[Int] {
    def write(input: Int): String = input.toString
  }

  implicit val serializeDouble = new Serialize[Double] {
    def write(input: Double): String = input.toString
  }

  implicit val serializeBoolean = new Serialize[Boolean] {
    def write(input: Boolean): String = if (input) "yes!" else "no!"
  }

  implicit def serializeList[A](
      implicit serialize: Serialize[A]
  ): Serialize[List[A]] = {
    new Serialize[List[A]] {
      def write(input: List[A]): String =
        "[" + input.map(serialize.write(_)).mkString(", ") + "]"
    }
  }

  implicit def serializeOption[A](
      implicit serialize: Serialize[A]
  ): Serialize[Option[A]] = {
    new Serialize[Option[A]] {
      def write(input: Option[A]): String =
        "Some(" + serialize.write(input.get) + ")"
    }
  }

  implicit def serializeSome[A](
      implicit serialize: Serialize[A]
  ): Serialize[Some[A]] = {
    new Serialize[Some[A]] {
      def write(input: Some[A]): String =
        "Some(" + serialize.write(input.get) + ")"
    }
  }

  implicit def serializeNone: Serialize[None.type] = {
    new Serialize[None.type] {
      def write(input: None.type): String = "None!"
    }
  }

  def write[A](v: A)(implicit s: Serialize[A]): String = s.write(v)
}

object TestThis extends App {
  import Serialize._
  println(write(1))
  println(write(2.0))
  println(write(false))
  println(write(List(1, 2)))
  println(write(Option(1)))
  println(write(Some(1)))
  println(write(None))

}
