package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.Matchers

class List$Test extends org.scalatest.FunSuite with Matchers {

  test("tail()") {
    tail(Nil) shouldBe Nil
    tail(List(1)) shouldBe Nil
    tail(List(1, 2)) shouldBe List(2)
    tail(List(1, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5)
  }

  test("setHead()") {
    setHead(Nil, 1) shouldBe Nil
    setHead(List(1), 2) shouldBe List(2)
    setHead(List(1, 2, 3), 0) shouldBe List(0, 2, 3)
  }

  test("drop()") {
    drop(Nil, 5) shouldBe Nil
    drop(List(1, 2), 1) shouldBe List(2)
    drop(List(1, 2, 3, 4, 5), 3) shouldBe List(4, 5)
    drop(List(1, 2, 3, 4, 5), 10) shouldBe Nil
  }

  test("dropWhile()") {
    dropWhile(Nil, (a: Int) => false) shouldBe Nil
    dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i > 5) shouldBe List(1, 2, 3, 4, 5)
    dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i < 3) shouldBe List(3, 4, 5)
  }

  test("init()") {
    init(Nil) shouldBe Nil
    init(List(1)) shouldBe Nil
    init(List(1, 2)) shouldBe List(1)
    init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
  }

  test("length()") {
    List.length(Nil) shouldBe 0
    List.length(List(1)) shouldBe 1
    List.length(List(1, 2, 3, 4, 5)) shouldBe 5
  }

  test("3.11 sum") {
    def sum(l: List[Int]): Int =
      foldLeft(l, 0)(_ + _)

    sum(Nil) shouldBe 0
    sum(List(1)) shouldBe 1
    sum(List(1, 2, 3, 4, 5)) shouldBe 15
    sum(List(1, 2, 3, 4, 5, -15)) shouldBe 0
  }

  test("3.11 product") {
    def product(l: List[Double]): Double =
      foldLeft(l, 1.0)(_ * _)

    product(Nil) shouldBe 1
    product(List(1)) shouldBe 1
    product(List(1, 2, 3, 4, 5)) shouldBe 120
    product(List(1, 2, 0, 4, 5)) shouldBe 0
  }

  test("3.11 length") {
    def length[A](l: List[A]): Int =
      foldLeft(l, 0)((length, _) => length + 1)

    length(Nil) shouldBe 0
    length(List(1)) shouldBe 1
    length(List(1, 2, 3, 4, 5)) shouldBe 5
  }

  test("append()") {
    append(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
    append(Nil, List(1, 2, 3)) shouldBe List(1, 2, 3)
    append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  test("appendFold()") {
    appendFold(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
    appendFold(Nil, List(1, 2, 3)) shouldBe List(1, 2, 3)
    appendFold(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  test("concat()") {
    concat(Nil) shouldBe Nil
    concat(List(Nil, Nil, Nil)) shouldBe Nil
    concat(List(List(1, 2, 3), Nil, Nil)) shouldBe List(1, 2, 3)
    concat(List(Nil, List(1, 2, 3), Nil)) shouldBe List(1, 2, 3)
    concat(List(Nil, Nil, List(1, 2, 3))) shouldBe List(1, 2, 3)
    concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  test("increment()") {
    increment(Nil) shouldBe Nil
    increment(List(1, 2, 3)) shouldBe List(2, 3, 4)
    increment(List(-2, -1, 0, 1, 2)) shouldBe List(-1, 0, 1, 2, 3)
  }

  test("dToS()") {
    dToS(Nil) shouldBe Nil
    dToS(List(1, 2.5, 3.1)) shouldBe List("1.0", "2.5", "3.1")
  }

  test("map()") {
    def addOne(a: Int): Int = a + 1

    map(List[Int]())(addOne) shouldBe Nil
    map(List(1, 2, 3))(addOne) shouldBe List(2, 3, 4)
    map(List(-2, -1, 0, 1, 2))(addOne) shouldBe List(-1, 0, 1, 2, 3)

    def toS[A](a: A): String = a.toString

    map(Nil)(toS) shouldBe Nil
    map(List(1, 2.5, 3.1))(toS) shouldBe List("1.0", "2.5", "3.1")
  }

  test("filter1()") {
    filter1(List(1, 2, 3, 4, 5, 6))(i => i % 2 == 0) shouldBe List(2, 4, 6)
  }

  test("flatMap()") {
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  test("filter()") {
    filter(List(1, 2, 3, 4, 5, 6))(i => i % 2 == 0) shouldBe List(2, 4, 6)
  }

  test("addLists()") {
    addLists(List(1, 2, 3), Nil) shouldBe Nil
    addLists(Nil, List(4, 5, 6)) shouldBe Nil
    addLists(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }

  test("zipWith()") {
    def add(a: Int, b: Int): Int = a + b

    zipWith(List(1, 2, 3), Nil)(add) shouldBe Nil
    zipWith(Nil, List(4, 5, 6))(add) shouldBe Nil
    zipWith(List(1, 2, 3), List(4, 5, 6))(add) shouldBe List(5, 7, 9)
  }

  test("hasSubsequence") {
    val list = List(1, 2, 3, 4)

    hasSubsequence(list, Nil) shouldBe true
    hasSubsequence(list, List(1)) shouldBe true
    hasSubsequence(list, List(2)) shouldBe true
    hasSubsequence(list, List(3)) shouldBe true
    hasSubsequence(list, List(4)) shouldBe true
    hasSubsequence(list, List(1, 2)) shouldBe true
    hasSubsequence(list, List(2, 3)) shouldBe true
    hasSubsequence(list, List(3, 4)) shouldBe true
    hasSubsequence(list, List(1, 2, 3)) shouldBe true
    hasSubsequence(list, List(2, 3, 4)) shouldBe true

    hasSubsequence(list, List(5)) shouldBe false
    hasSubsequence(list, List(1, 3)) shouldBe false
    hasSubsequence(list, List(1, 3, 4)) shouldBe false
  }

}
