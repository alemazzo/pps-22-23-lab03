package u03

import u02.Modules.Person
import u02.Modules.Person.Teacher
import u02.Optionals.Option
import u02.Optionals.Option.*

import scala.annotation.tailrec

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:
    
    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0


    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (l, 0) => l
      case (Cons(_, t), i) => drop(t, i - 1)
      case _ => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), r) => r
      case (Cons(h, Nil()), r) => Cons(h, r)
      case (Cons(h, t), r) => Cons(h, append(t, r))

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), flatMap(t)(x => Cons(mapper(x), Nil())))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => flatMap(t)(x => if(pred(x)) Cons(x, Nil()) else Nil())
      case Nil() => Nil()

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => max(t) match
        case None() => Some(h)
        case Some(a) => Some(Integer.max(h, a))
      case Nil() => None()

    def peopleToCourses(l: List[Person]): List[String] = flatMap(l)(_ match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    )

    def foldLeft[A, B](l: List[A])(d: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(d, h))(f)
      case Nil() => d

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
