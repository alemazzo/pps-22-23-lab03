package u03

import scala.annotation.tailrec
import u02.Optionals.Option
import u02.Optionals.Option.{None, Some}

import java.util
import java.util.Collections

object Lab03 {

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:

    // 1a. Drop

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (l, 0) => l
      case (Cons(_, t), n) => drop(t, n-1)
      case _ => Nil()

    // 1b. Append

    def append[A](l: List[A], r: List[A]): List[A] = l match
      case Nil() => r
      case Cons(h, t) => Cons(h, append(t, r))

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

    def reverse[A](l: List[A]): List[A] =
      foldLeft(l)(Nil(): List[A])((acc, h) => Cons(h, acc))

    def foldRight[A, B](l: List[A])(b: B)(op: (A, B) => B): B =
      foldLeft(reverse(l))(b)((acc, h) => op(h, acc))

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

  // Streams
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // 5. Stream Drop

    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (l, 0) => l
      case (Cons(_, tail), n) if n > 0 => drop(tail())(n - 1)
      case _ => Empty()

}