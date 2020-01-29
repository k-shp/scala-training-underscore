import cats.implicits._
import cats.{Semigroup, Monoid, Functor, Apply, Applicative, Monad, Foldable, Now, Later}

// Semigroup: has `combine` method defined
// `combine` takes two values of type A and returns one value of type A

Semigroup[Int].combine(1, 2) == 3
Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6))  == List(1,2,3,4,5,6)
Semigroup[Option[Int]].combine(Option(1), Option(2)) == Some(3)
Semigroup[Option[Int]].combine(Option(1), None) == Some(1)
Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) == 67

Map("foo" -> Map("bar" -> 5)).combine(Map("foo" -> Map("bar" -> 6), "baz" -> Map()))
Map("foo" -> List(1, 2)).combine(Map("foo" -> List(3, 4), "bar" -> List(42)))

// Monoid: extends Semigroup with `empty` method defined
// `empty` is like an identity? i.e. when combined with any value of that type it returns that value
// e.g. in Monoid[String] with `combine` defined as concatenation `empty` = ""
// this allows to combine elements of potentially empty collection of T and return a T instead of Option[T] as we have a sensible default

Monoid[String].empty == ""
Monoid[String].combineAll(List("a", "b", "c")) == "abc"
Monoid[String].combineAll(List()) == ""

Monoid[Map[String, Int]].combineAll(List(Map("a" → 1, "b" → 2), Map("a" → 3))) == Map("a" -> 4, "b" -> 2)
Monoid[Map[String, Int]].combineAll(List()) == Map()

val l = List(1, 2, 3, 4, 5)
l.foldMap(identity) == 15
l.foldMap(i ⇒ i.toString) == "12345"

implicit def monoidTuple[A: Monoid, B: Monoid]: Monoid[(A, B)] =
  new Monoid[(A, B)] {
    def combine(x: (A, B), y: (A, B)): (A, B) = {
      val (xa, xb) = x
      val (ya, yb) = y
      (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
    }
    def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
  }

l.foldMap(i => (i, i * 2.0)) == (15, 30.0)

// Functor: type class with one "hole" F[?], e.g. Option, Future, List
// has `map` operation defined with signature def map[A, B](fa: F[A])(f: A => B): F[B]
// if no `map` on types, can use `andThen` to implement `map`

implicit val optionFunctor: Functor[Option] = new Functor[Option] {
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
}

// Don't get this :/
//implicit def function1Functor[In]: Functor[Function1[In, ?]] = {
//  new Functor[Function1[In, ?]] {
//    def map[A,B](fa: In => A)(f: A => B): Function1[In, B] = fa andThen f
//  }
//}

// Option is a functor which only applies the function when Option value is Some
Functor[Option].map(Option("hello"))(_.length) == Some(5)
Functor[Option].map(None: Option[String])(_.length) == None

// can use Functor to `lift` a function from A => B to F[A] => F[B]
val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
lenOption(Some("hello")) == Some(5)
lenOption(None: Option[String]) == None

// Functor provides `fproduct` which pairs a value with applying a function to that value
val source = List("Cats", "is", "awesome")
val product = Functor[List].fproduct(source)(_.length).toMap
product.get("Cats").getOrElse(0) == 4
product.get("Dogs").getOrElse(0) == 0

// Functors compose - Given any functors F[_] and G[_], we can create a new functor F[G[_]]
val optList = Functor[List] compose Functor[Option]
optList.map(List(Some(1), None, Some(2)))(_ + 1) == List(Some(2), None, Some(3))

// Apply extends Functor, has function `ap`
// while `map` does A => B transformation, `ap` does F[A => B]

val intToString: Int => String = _.toString
val double: Int => Int = _ * 2
val addTwo: Int => Int = _ + 2

Apply[Option].map(Some(1))(intToString) == Some("1")
Apply[Option].map(Some(1))(double) == Some(2)

// Apply instances also Compose like Functors
val listOpt = Apply[List] compose Apply[Option]
val plusOne = (x: Int) => x + 1
listOpt.ap(List(Some(plusOne)))(List(Some(1), None)) == List(Some(2), None)

Apply[Option].ap(Some(intToString))(Some(1)) == Some("1")
Apply[Option].ap(Some(double))(Some(1)) == Some(2)
Apply[Option].ap(Some(double))(None) == None
Apply[Option].ap(None)(Some(1)) == None
Apply[Option].ap(None)(None) == None

// functions apN accept N arguments where ap accepts 1
val addArity2 = (a: Int, b: Int) => a + b
Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) == Some(3)
Apply[Option].ap2(Some(addArity2))(Some(1), None) == None
val addArity3 = (a: Int, b: Int, c: Int) => a + b + c
Apply[Option].ap3(Some(addArity3))(Some(1), Some(3), Some(6)) == Some(10)

Apply[Option].map2(Some(1), Some(2))(addArity2) == Some(3)
Apply[Option].tuple2(Some(1), Some(2)) == Some((1, 2))

// |@| operator

val option2 = (Option(1), Option(2))
val option3 = (option2._1, option2._2, Option.empty[Int])

option2 mapN addArity2
option3 mapN addArity3

option2 apWith Some(addArity2)
option3 apWith Some(addArity3)

option2.tupled
option3.tupled

// Applicative extends Apply by adding `pure` method
// def pure[A](x: A): F[A]
// `pure` takes any value and returns it in context of the Functor

Applicative[Option].pure(1) == Some(1)
Applicative[List].pure(1) == List(1)
(Applicative[List] compose Applicative[Option]).pure(1) == List(Option(1))

// Applicative is a generalization of Monad
Monad[Option].pure(1) == Some(1)
Applicative[Option].pure(1) == Option(1)

// Monad extends Applicative with a `flatten` method
// If Applicative is already present and had a `flatten` method, can extend it to Monad implementing:
// 1) `pure` (can be reused from Applicative)
// 2) `flatMap` (can use `flatten` to define it, `flatMap` is just `map` followed by `flatten`, and `flatten` is just `flatMap` using an identity function x => x i.e. flatMap(_)(x => x)

/* none of below compiles*/
//implicit def optionMonad(implicit app: Applicative[Option]) =
//  new Monad[Option] {
//    //define flatMap using Option's flatten Method
//    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = app.map(fa)(f).flatten
//    override def pure[A](a: A): Option[A] = app.pure(a)
//
//  }

//Monad[Option].pure(42) == Option(42)
//implicit val listMonad = new Monad[List] {
//  def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
//  def pure[A](a: A): List[A] = List(a)
//}

Monad[List].flatMap(List(1, 2))(x ⇒ List(x, x)) == List(1, 1, 2, 2)
Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) == Some("truthy")
Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4))

// Composition: Unlike Functors and Applicatives, you cannot derive a monad instance for a generic M[N[_]] where both M[_] and N[_] have an instance of a monad.
//However, it is common to want to compose the effects of both M[_] and N[_]. One way of expressing this is to provide instructions on how to compose any outer monad (F in the following example) with a specific inner monad (Option in the following example).

// Foldable: Foldable type class instances can be defined on for data structures that can be folded to a summary value
// For collections, these methods will combine values to produce a single result

// foldLeft is an eager left-associative fold on F using the given function
Foldable[List].foldLeft(List(1,2,3), 0)(_ + _)

// foldRight is a lazy right-associative fold on F using given function
// has signature (A. Eval[B]) => Eval[B] to support laziness
val lazyResult = Foldable[List].foldRight(List(1,2,3), Now(0))((x, rest) => Later(x + rest.value))
lazyResult.value == 6

// fold aka combineAll combines every value in the foldable using given Monoid instance
Foldable[List].fold(List(1,2,3)) == 6

// foldMap is similar to fold but maps every A into B and then combines them using Monoid[B]
Foldable[List].foldMap(List(1,2,3))(_.toString) == "123"

// foldK is similar to fold but combines every value in the foldable using the given MonoidK[G] instance instead of Monoid[G]
Foldable[List].foldK(List(List(1, 2), List(3, 4, 5)))

Foldable[List].find(List(1, 2, 3))(_ > 5) == None

Foldable[Option].toList(Option(42)) == List(42)
Foldable[Option].toList(None) == List()

Foldable[List].filter_(List(1, 2, 3))(_ < 3) == List(1,2)
Foldable[Option].filter_(Option(42))(_ != 42) == List()

// traverse_ : traverse the foldable mapping A values to G[B] and combining them using Applicative[G] and discarding the results
// primarily useful when G[_] represents action or effect and specific B aspect of G[B] is not needed
// B will be discarded and Unit returned instead
def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
Foldable[List].traverse_(List("1", "2", "3"))(parseInt)
Foldable[List].traverse_(List("a", "b"))(parseInt) == None
Foldable[List].traverse_(List("a", "2"))(parseInt)

val FoldableListOption = Foldable[List].compose[Option]
FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) == 10
FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))) == "123"

Foldable[List].foldK(List(None, Option("two"), Option("three")))






// Questions
// What's type class vs type?
// combine vs combineAll?
// ap vs map on Apply?
// why a default value in foldLeft and foldRight but not in fold?
// why filter_ and traverse_ and not filter and traverse on foldable?


println("done!")
