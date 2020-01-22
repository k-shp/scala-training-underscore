case class Customer(customerId: Int, customerAge: Int)

object Customer {
  implicit val customerOrdering: Ordering[Customer] = {
    new Ordering[Customer] {
      def compare(x: Customer, y: Customer): Int = {
        x.customerId.compareTo(y.customerId)
      }
    }
  }
}

object ImplicitsOrderingTest {
  def main(args: Array[String]): Unit = {
    val otherOrdering: Ordering[Customer] = {
      new Ordering[Customer] {
        def compare(x: Customer, y: Customer): Int = {
          x.customerAge.compareTo(y.customerAge)
        }
      }
    }
    val customers = List(Customer(2, 10), Customer(3, 9), Customer(1, 8))
    println(customers.sorted)
    println(customers.sorted(otherOrdering))

  }
}
