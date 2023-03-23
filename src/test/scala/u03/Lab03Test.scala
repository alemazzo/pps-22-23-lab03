package u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Lab03.List
import u03.Lab03.List.*
import u03.Lab03.Person
import u03.Lab03.Person.*
import u03.Lab03.Stream;
import u02.Optionals.Option
import u02.Optionals.Option.{Some, None}

class Lab03Test {


  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testDrop() =
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 3))
    assertEquals(Nil(), drop(l, 4))
    assertEquals(Nil(), drop(l, -2))


  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, Cons(40, Nil())))
    assertEquals(l, append(l, Nil()))
    assertEquals(Cons(40, Nil()), append(Nil(), Cons(40, Nil())))
    assertEquals(Nil(), append(Nil(), Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(x => Cons(x + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(x => Cons(x + 1, Cons(x + 2, Nil()))))
    assertEquals(Nil(), flatMap(Nil[Int]())(x => Cons(x + 1, Nil())))
    assertEquals(Nil(), flatMap(l)(x => Nil()))


  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))
    assertEquals(Nil(), map(Nil[Int]())(_ + 1))
    //TODO: IS IT SUPPOSED TO WORK? ====> assertEquals(Nil(), map(l)(_ => Nil()))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
    assertEquals(Nil(), filter(l)(_ > 30))
    assertEquals(Nil(), filter(Nil[Int]())(_ > 30))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testMax() =
    assertEquals(Some(30), max(l))
    assertEquals(None(), max(Nil()))
    assertEquals(Some(10), max(Cons(10, Nil())))

  @Test def testPeopleToCourses() =
    val list = Cons(Teacher("Mirko", "OOP"), Cons(Student("Alessandro", 2000), Cons(Teacher("Gianluca", "PPS"), Nil())))
    assertEquals(Cons("OOP", Cons("PPS", Nil())), toCourses(list))
    assertEquals(Nil(), toCourses(Nil[Person]()))

  @Test def testFoldLeft() =
    assertEquals(60, foldLeft(l)(0)(_ + _))
    assertEquals(-60, foldLeft(l)(0)(_ - _))
    assertEquals(0, foldLeft(Nil[Int]())(0)(_ + _))

  @Test def testReverse() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(l))
    assertEquals(Nil(), reverse(Nil[Int]()))
    assertEquals(Cons(10, Nil()), reverse(Cons(10, Nil())))
    assertEquals(l, reverse(reverse(l)))

  @Test def testFoldRight() =
    assertEquals(60, foldRight(l)(0)(_ + _))
    assertEquals(20, foldRight(l)(0)(_ - _))
    assertEquals(0, foldRight(Nil[Int]())(0)(_ + _))
    val list = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-8, foldRight(list)(0)(_ - _))


  @Test def testStreamDrop() =
    val stream = Stream.iterate(0)(_ + 1)
    assertEquals(Cons(2, Cons(3, Cons(4, Nil()))), Stream.toList(Stream.take(Stream.drop(stream)(2))(3)))
    assertEquals(Nil(), Stream.toList(Stream.take(Stream.drop(stream)(2))(0)))
    assertEquals(Nil(), Stream.toList(Stream.take(Stream.drop(stream)(2))(-1)))

  @Test def testStreamConstant() =
    assertEquals(Cons(3, Cons(3, Cons(3, Nil()))), Stream.toList(Stream.take(Stream.constant(3))(3)))
    assertEquals(Nil(), Stream.toList(Stream.take(Stream.constant(3))(0)))
    assertEquals(Nil(), Stream.toList(Stream.take(Stream.constant(3))(-1)))
    
}
