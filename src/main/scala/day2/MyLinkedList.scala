package day2

sealed trait MyLinkedList[A] {
  def head: Option[A]
  def tail: Option[MyLinkedList[A]]
  def length: Int
  def filter(predicate: A => Boolean): MyLinkedList[A]
  def map[B](func: A => B): MyLinkedList[B]
  def find(predicate: A => Boolean): Option[A]
  def append(list: MyLinkedList[A]): MyLinkedList[A]
  def flatMap[B](func: A => MyLinkedList[B]): MyLinkedList[B]

}
case class MyEmptyList[A]() extends MyLinkedList[A] {
  def head: Option[A] = None
  def tail: Option[MyLinkedList[A]] = None
  def isEmpty: Boolean = true
  def length: Int = 0
  def filter(predicate: A => Boolean): MyLinkedList[A] = MyEmptyList[A]
  def map[B](func: A => B): MyLinkedList[B] = MyEmptyList[B]
  def find(predicate: A => Boolean): Option[A] = None
  def append(list: MyLinkedList[A]): MyLinkedList[A] = list
  def flatMap[B](func: A => MyLinkedList[B]): MyLinkedList[B] = MyEmptyList[B]
}
case class MyFullList[A](first: A, rest: MyLinkedList[A])
    extends MyLinkedList[A] {
  def head: Option[A] = Some(first)

  def tail: Option[MyLinkedList[A]] = Some(rest)

  def isEmpty: Boolean = false

  def length: Int = {
    1 + rest.length
  }

  def filter(predicate: A => Boolean): MyLinkedList[A] =
    predicate(first) match {
      case true  => MyFullList(first, rest.filter(predicate))
      case false => rest.filter(predicate)
    }

  def map[B](func: A => B): MyLinkedList[B] =
    MyFullList(func(first), rest.map(func))

  def find(predicate: A => Boolean): Option[A] = predicate(first) match {
    case true  => Some(first)
    case false => rest.find(predicate)
  }

  def append(anotherList: MyLinkedList[A]): MyLinkedList[A] = {
    MyFullList(first, this.rest.append(anotherList))
  }

  def flatMap[B](func: A => MyLinkedList[B]): MyLinkedList[B] = {
    func(first).append(rest.flatMap(func))
  }
}
object MyLinkedListTest {
  def main(args: Array[String]): Unit = {
    val testList = MyFullList[Int](5, MyFullList(10, MyEmptyList[Int]))

    println(testList.first)
    println(testList.tail)
    println(testList.isEmpty)
    println(testList.length)
    println(testList.filter(_ > 6))
    println(testList.map(_ * 2.0))
    println(testList.find(_ < 7))
    println(testList.flatMap(MyFullList[Int](_, MyEmptyList[Int])))
    println(testList.map(MyFullList[Int](_, MyEmptyList[Int])))

  }

}
