package com.thaj.functionalprogramming.example

import org.specs2._
import org.scalatest.prop.PropertyChecks
import org.scalatest.MustMatchers._
import org.scalatest.WordSpec

class ListOperations extends Specification {

  import com.thaj.functionalprogramming.example.exercises.TypeAndDataConstructors._, List._

  def is =
    s"""
       |reverse list should reverse the list ${reverseList(List(1, 2, 3)) == List(3, 2, 1)}
       |product list shoud do the multiplication ${productFoldLeft(List(1, 2, 3)) == 6}
       |append a list with another list ${appendWithFoldRight(List(1, 2, 3), List(3, 3, 3)) == List(1, 2, 3, 3, 3, 3)}
       |append a list with another list ${appendWithFoldLeft(List(1, 2, 3), List(3, 3, 3)) == List(1, 2, 3, 3, 3, 3)}
       |test foldLeftViaFoldRight ${foldLeftViaFoldRightBetterWay(List(1,2,3 ), 0)((a, b) => a + 1 ) ==  foldLeft(List(1,2,3), 0)((a,b) => b + 1)}
       |an interesting problem is tested here using foldRight, where we confirm it is folding from right $printFolRight
       |concatenate list of list ${concantenateListOfList(List(List(1,2,3), List(1,2,3))) == List(1,2,3,1,2,3)}
       |concatenate list of list ${concantenateListOfList(List(Nil, List(1,2,3))) == List(1,2,3)}
       |concatenate list of list ${concantenateListOfList(List(List(1, 2), Nil, List(2,3)))  == List(1, 2, 2, 3)}
       |test add 1 to each element ${add1ToEachElement(List(1,2,3)) == List(2,3,4)}
       |test covert double to String ${convertEachDoubleToString(List(1,2,3)) == List("1.0", "2.0", "3.0")}
       |test map  ${List.map(List(1,2,3))((a: Int) => a + 1) == List(2,3,4)}
       |test filter ${filter(List(1,2,3))((a: Int) => a == 2) == List(2)}
     """.stripMargin

  def printFolRight = {
    val s = foldRight1(List(1, 3), 0)((a, b) => {
      val c = {
        a + 1
      };  c
    }); s == 2
  }

  def foldRight1[A, B](list: List[A], init: B)(f: (A, B) => B): B = list match {
    case Nil => init
    case Cons(x, xs) => f(x, foldRight1(xs, init)(f))
  }
}

