package day2

sealed trait MyLinkedListPM[A] {
  def head: Option[A] = this match {
    case _: MyEmptyListPM[A] => None
    case m: MyFullListPM[A]  => Some(m.first)
  }
  def tail: Option[MyLinkedListPM[A]] = this match {
    case _: MyEmptyListPM[A] => None
    case m: MyFullListPM[A]  => Some(m.rest)
  }

  def isEmpty: Boolean = this match {
    case _: MyEmptyListPM[A] => true
    case m: MyFullListPM[A]  => false
  }

  def length: Int = this match {
    case _: MyEmptyListPM[A] => 0
    case m: MyFullListPM[A]  => 1 + m.rest.length
  }
  def filter(predicate: A => Boolean): MyLinkedListPM[A] = this match {
    case _: MyEmptyListPM[A] => MyEmptyListPM[A]
    case m: MyFullListPM[A] =>
      predicate(m.first) match {
        case true  => MyFullListPM(m.first, m.rest.filter(predicate))
        case false => m.rest.filter(predicate)
      }
  }
  def map[B](func: A => B): MyLinkedListPM[B] = this match {
    case _: MyEmptyListPM[A] => MyEmptyListPM[B]
    case m: MyFullListPM[A]  => MyFullListPM(func(m.first), m.rest.map(func))
  }
  def find(predicate: A => Boolean): Option[A] = this match {
    case _: MyEmptyListPM[A] => None
    case m: MyFullListPM[A] =>
      predicate(m.first) match {
        case true  => Some(m.first)
        case false => m.rest.find(predicate)
      }
  }
  def append(list: MyLinkedListPM[A]): MyLinkedListPM[A] = this match {
    case _: MyEmptyListPM[A] => list
    case m: MyFullListPM[A]  => MyFullListPM(m.first, m.rest.append(list))
  }
  def flatMap[B](func: A => MyLinkedListPM[B]): MyLinkedListPM[B] = this match {
    case _: MyEmptyListPM[A] => MyEmptyListPM[B]
    case m: MyFullListPM[A]  => func(m.first).append(m.rest.flatMap(func))
  }
}

case class MyEmptyListPM[A]() extends MyLinkedListPM[A]

case class MyFullListPM[A](first: A, rest: MyLinkedListPM[A])
    extends MyLinkedListPM[A]

object MyLinkedListPMTest {
  def main(args: Array[String]): Unit = {
    val testList = MyFullListPM[Int](5, MyFullListPM(10, MyEmptyListPM[Int]))
    println(testList.first)
    println(testList.tail)
    println(testList.isEmpty)
    println(testList.length)
    println(testList.filter(_ > 6))
    println(testList.map(_ * 2.0))
    println(testList.find(_ < 7))
    println(testList.flatMap(MyFullListPM[Int](_, MyEmptyListPM[Int])))
    println(testList.map(MyFullListPM[Int](_, MyEmptyListPM[Int])))
  }
}
