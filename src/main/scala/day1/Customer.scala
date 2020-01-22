package day1

sealed trait Customer
final case class User(id: Int)
final case class Rider(name: String, vehicle: Vehicle)
final case class Restaurant(name: String, licensed: Boolean)

sealed trait Vehicle {
  def isMotorised = this match {
    case Bicycle => false
    case _       => true
  }
}

final case object Bicycle extends Vehicle
final case object Scooter extends Vehicle
final case object Moped extends Vehicle

object Bla {
  def main(args: Array[String]): Unit = {
    val a = Map[String, Int]("lol" -> 2, "bla" -> 3)
    println(a.get("lol"))
  }
}
