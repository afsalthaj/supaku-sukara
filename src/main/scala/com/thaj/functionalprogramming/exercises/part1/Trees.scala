package com.thaj.functionalprogramming.example.exercises

/**
 * Created by afsalthaj on 17/10/2016.
 */
// ADTS are algebraic data types, where the type is constructed (sum or union of) by multiple data constructors
// A few more cool examples of playing with ADTs
sealed trait Tree[+A] {
  // Exercise 3.28
  // Write a function map, analogous to the method of the same name on List,
  // that modifies each element in a tree with a given function.
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(x)      => Leaf(f(x))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]

object Tree {
  // Exercise 3.25
  // Write a function size that counts the number of nodes (leaves and branches) in a tree.
  // But this seem to be a bit tricky, it isnt tail recursive. But let's go with this, assuming that
  // a tree is a tree and not a big nested monster!
  def getSizeOfTrees[A](a: Tree[A]): Int = a match {
    case Leaf(_)      => 1
    case Branch(l, r) => getSizeOfTrees(l) + getSizeOfTrees(r) + 1
  }

  // Exercise 3.26
  // Write a function maximum that returns the maximum element in a
  // Tree[Int]. (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
  def maxi(tree: Tree[Int]): Int = tree match {
    case Leaf(a)      => a
    case Branch(l, r) => maxi(l).max(maxi(r))
  }

  // Exercise 3.27
  // Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  //Branch(Leaf(1), Branch(Leaf(2), Leaf(3))
  def depth[A](a: Tree[A]): Int = {
    a match {
      case Leaf(_)      => 0
      case Branch(l, r) => depth(l).max(depth(r) + 1)
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x)      => Leaf(f(x))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  // Exercise 3.29
  // Generalize size, maximum, depth, and map, writing a new
  // function fold that abstracts over their similarities. Reimplement
  // them in terms of this more general function. Can you draw an analogy between
  // this fold function and the left and right folds for List?
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeWithFold[A](tree: Tree[A]) = fold(tree)(a => 1)(_ + _ + 1)

  def maxiWithFold(tree: Tree[Int]) = fold(tree)(a => a)(_ max _)

  def depthWithFold[A](tree: Tree[A]) = fold(tree)(a => 0)(_ max _ + 1)

  // Basically I dont want things to be this complex, we will look at it later!
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
