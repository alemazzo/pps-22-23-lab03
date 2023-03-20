package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.Modules.Person
import u02.Modules.Person.{Student, Teacher}
import u02.Optionals.Option.{None, Some}

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_+""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))

  @Test def testDrop() =
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 3))
    assertEquals(Nil(), drop(l, 4))
    assertEquals(Nil(), drop(l, -2))

  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, Cons(40, Nil())))
    assertEquals(l, append(l, Nil()))
    assertEquals(Cons(40, Nil()), append(Nil(), Cons(40, Nil())))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(x => Cons(x + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(x => Cons(x + 1, Cons(x + 2, Nil()))))

  @Test def testMax() =
    assertEquals(Some(30), max(l))
    assertEquals(None(), max(Nil()))

  @Test def testPeopleToCourses() =
    val list = Cons(Teacher("Mirko", "OOP"), Cons(Student("Alessandro", 2000), Cons(Teacher("Gianluca", "PPS"), Nil())))
    assertEquals(Cons("OOP", Cons("PPS", Nil())), peopleToCourses(list))

  @Test def testFoldLeft() =
    assertEquals(60, foldLeft(l)(0)(_+_))
    assertEquals(-60, foldLeft(l)(0)(_-_))
    assertEquals(0, foldLeft(Nil[Int]())(0)(_+_))

  @Test def testFoldRight() =
    assertEquals(60, foldRight(l)(0)(_+_))
    assertEquals(20, foldRight(l)(0)(_-_))
    assertEquals(0, foldRight(Nil[Int]())(0)(_+_))



