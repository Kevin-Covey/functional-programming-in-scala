package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.Matchers

class List$Test extends org.scalatest.FunSuite with Matchers {

  test("tail()") {
    tail(Nil) should equal(Nil)
    tail(List(1)) should equal(Nil)
    tail(List(1, 2)) should equal(List(2))
    tail(List(1, 2, 3, 4, 5)) should equal(List(2, 3, 4, 5))
  }

  test("setHead()") {
    setHead(Nil, 1) should equal(Nil)
    setHead(List(1), 2) should equal(List(2))
    setHead(List(1, 2, 3), 0) should equal(List(0, 2, 3))
  }

  test("drop()") {
    drop(Nil, 5) should equal(Nil)
    drop(List(1, 2), 1) should equal(List(2))
    drop(List(1, 2, 3, 4, 5), 3) should equal(List(4, 5))
    drop(List(1, 2, 3, 4, 5), 10) should equal(Nil)
  }

  test("dropWhile()") {
    dropWhile(Nil, (a: Int) => false) should equal(Nil)
    dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i > 5) should equal(List(1, 2, 3, 4, 5))
    dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i < 3) should equal(List(3, 4, 5))
  }

  test("init()") {
    init(Nil) should equal(Nil)
    init(List(1)) should equal(Nil)
    init(List(1, 2)) should equal(List(1))
    init(List(1, 2, 3, 4, 5)) should equal(List(1, 2, 3, 4))
  }

  test("length()") {
    List.length(Nil) should equal(0)
    List.length(List(1)) should equal(1)
    List.length(List(1, 2, 3, 4, 5)) should equal(5)
  }

  test("3.11 sum") {
    def sum(l: List[Int]): Int =
      foldLeft(l, 0)(_ + _)

    sum(Nil) should equal(0)
    sum(List(1)) should equal(1)
    sum(List(1, 2, 3, 4, 5)) should equal(15)
    sum(List(1, 2, 3, 4, 5, -15)) should equal(0)
  }

  test("3.11 product") {
    def product(l: List[Double]): Double =
      foldLeft(l, 1.0)(_ * _)

    product(Nil) should equal(1)
    product(List(1)) should equal(1)
    product(List(1, 2, 3, 4, 5)) should equal(120)
    product(List(1, 2, 0, 4, 5)) should equal(0)
  }

  test("3.11 length") {
    def length[A](l: List[A]): Int =
      foldLeft(l, 0)((length, _) => length + 1)

    length(Nil) should equal(0)
    length(List(1)) should equal(1)
    length(List(1, 2, 3, 4, 5)) should equal(5)
  }

  test("append()") {
    append(List(1, 2, 3), Nil) should equal(List(1, 2, 3))
    append(Nil, List(1, 2, 3)) should equal(List(1, 2, 3))
    append(List(1, 2, 3), List(4, 5, 6)) should equal(List(1, 2, 3, 4, 5, 6))
  }

  test("appendFold()") {
    appendFold(List(1, 2, 3), Nil) should equal(List(1, 2, 3))
    appendFold(Nil, List(1, 2, 3)) should equal(List(1, 2, 3))
    appendFold(List(1, 2, 3), List(4, 5, 6)) should equal(List(1, 2, 3, 4, 5, 6))
  }

  test("concat()") {
    concat(Nil) should equal(Nil)
    concat(List(Nil, Nil, Nil)) should equal(Nil)
    concat(List(List(1, 2, 3), Nil, Nil)) should equal(List(1, 2, 3))
    concat(List(Nil, List(1, 2, 3), Nil)) should equal(List(1, 2, 3))
    concat(List(Nil, Nil, List(1, 2, 3))) should equal(List(1, 2, 3))
    concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) should equal(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("increment()") {
    increment(Nil) should equal(Nil)
    increment(List(1, 2, 3)) should equal(List(2, 3, 4))
    increment(List(-2, -1, 0, 1, 2)) should equal(List(-1, 0, 1, 2, 3))
  }

  test("dToS()") {
    dToS(Nil) should equal(Nil)
    dToS(List(1, 2.5, 3.1)) should equal(List("1.0", "2.5", "3.1"))
  }

  test("map()") {
    def addOne(a: Int): Int = a + 1

    map(List[Int]())(addOne) should equal(Nil)
    map(List(1, 2, 3))(addOne) should equal(List(2, 3, 4))
    map(List(-2, -1, 0, 1, 2))(addOne) should equal(List(-1, 0, 1, 2, 3))

    def toS[A](a: A): String = a.toString
    map(Nil)(toS) should equal(Nil)
    map(List(1, 2.5, 3.1))(toS) should equal(List("1.0", "2.5", "3.1"))
  }

}
