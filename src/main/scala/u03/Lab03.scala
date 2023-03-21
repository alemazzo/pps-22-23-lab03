package u03

import scala.annotation.tailrec
import u02.Optionals.Option
import u02.Optionals.Option.{Some, None}

object Lab03 {

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:

    // 1a. Drop

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Cons(h, t), 0) => Cons(h, t)
      case (Cons(_, t), n) => drop(t, n-1)
      case _ => Nil()

    // 1b. Append

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Nil() => right
      case Cons(h, t) => Cons(h, append(t, right))

    // 1c. Flatmap

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    // 1d. Map & Filter (using flatMap)

    def map[A, B](l: List[A])(f: A => B): List[B] =
      flatMap(l)(x => Cons(f(x), Nil()))

    def filter[A](l: List[A])(p: A => Boolean): List[A] =
      flatMap(l)(e => p(e) match
        case true => Cons(e, Nil())
        case false => Nil()
      )

    // 2. Max

    def max(l: List[Int]): Option[Int] = l match
      case Nil() => None()
      case Cons(h, t) => max(t) match
        case None() => Some(h)
        case Some(m) => Some(h max m)

    // 4. Fold left & right

    @tailrec
    def foldLeft[A, B](l: List[A])(b: B)(op: (B, A) => B): B = l match
      case Nil() => b
      case Cons(h, t) => foldLeft(t)(op(b, h))(op)

    def foldRight[A, B](l: List[A])(b: B)(op: (A, B) => B): B =
      foldLeft(l)(b)((b, a) => op(a, b))

  // 3. People to Courses

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:

    import List.*

    def toCourses(l: List[Person]): List[String] =
      flatMap(l)(_ match
        case Student(_, _) => Nil()
        case Teacher(_, c) => Cons(c, Nil())
      )

}
