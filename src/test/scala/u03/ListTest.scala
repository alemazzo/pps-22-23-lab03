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



