package com.thaj.functionalprogramming.example

import org.specs2.Specification

class TreeSpec extends Specification {

  import com.thaj.functionalprogramming.example.exercises._, Tree._

  def is =
    s"""
       |count the size of Tree ${getSizeOfTrees(Branch(Leaf(1), Leaf(2))) == 3}
       |get the max of Tree ${maxi(Leaf(1)) == 1}
       |get the max of Tree ${maxi(Branch(Leaf(1), Leaf(2))) == 2}
       |get the max of Tree ${maxi(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3}
       |get the max of depth ${depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3}
       |get the max of depth ${depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(1), Leaf(2))))) == 4}
       |test the map function ${Leaf(1).map(_ + 2) == Leaf(3)}
       |test the map function ${Branch(Leaf(1), Leaf(2)).map(_ + 2) == Branch(Leaf(3), Leaf(4))}
     """.stripMargin
}