package com.thaj.functionalprogramming.example.exercises

/**
 * Created by afsalthaj on 12/10/16.
 */
// Covers data sharing - FP removes unnecessary copying of data.
// Covers variadic functions (apply method, accepting zero or more arguments of type A)
object TypeAndDataConstructors {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(a: List[Int]): Int = a match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
    }

    def product(a: List[Double]): Double = a match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](a: A*): List[A] =
      if (a.isEmpty)
        Nil
      else
        Cons(a.head, apply(a.tail: _*))

    // Performance clearly depends on the number of elements, seems like we are creating new objects.
    def init[A](a: List[A]): List[A] = a match {
      case Nil => Nil
      case Cons(head1, Cons(head2, Nil)) => Cons(head1, Nil)
      case Cons(head, tail) => Cons(head, init(tail))
    }

    // Well, I think I love exceptions too.. Mmm.. May be not... Ah this is crazy!
    // It is constant time performance
    def head[A](a: List[A]): A = a match {
      case Nil => throw new Exception("head of empty list")
      case Cons(head, _) => head
    }

    // Note that the function takes constant time.
    // There is no copy of data. This means List(1,2,3,4)
    // and List(2,3,4) share the same memory of data.
    // It is constant time performance
    def tail[A](a: List[A]): List[A] = a match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

    // In any of these functions, you are not copying data.
    // It is clear reuse of data that exist in the same memory location
    // Seems like I am not copying data here
    def drop[A](l: List[A], n: Int): List[A] = {
      def go(a: List[A], inc: Int): List[A] = {
        if (inc >= n) a
        else
          a match {
            case Nil => Nil
            case Cons(_, tail) => go(tail, inc + 1)
          }
      }

      go(l, 0)
    }

    // An awesome higher order functions! Believe me, it is awesome!
    // Seems like I am copying data :(
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t1) if f(h) => dropWhile(t1, f)
      case _ => l
    }

    // It’s a little unfortunate that we need to state that the type of x is Int.
    // The first argument to dropWhile is a List[Int], so the function in the second argument must accept an Int.
    // Scala can infer this fact if we group dropWhile into two argument lists:
    def dropWhileClean[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhileClean(t)(f)
      case _ => as
    }


    // appending without any copy operations
    // Note that this definition only copies values
    // until the first list is exhausted, so its
    // runtime and memory usage are determined only by the length of a1.
    // The remaining list then just points to a2. If we were to implement this same
    // function for two arrays, we’d be forced to copy all the elements in both arrays into the result.
    // In this case, the immutable linked list is much more efficient than an array!
    def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    // Exercise 3.7
    // Can product, implemented using foldRight,
    // immediately halt the recursion and return 0.0 if
    // it encounters a 0.0? Why or why not?
    // Consider how any short-circuiting might work if you call foldRight with a large list.
    // This is a deeper question that we’ll return to in chapter 5.
    // Answer: It will continue to execute even it encounters 0

    // the magical foldRight, folding it from right to left; however, it results in stack overflow error
    def foldRight[A, B](list: List[A], init: B)(f: (A, B) => B): B = list match {
      case Nil => init
      case Cons(x, xs) => f(x, foldRight(xs, init)(f))
    }

    // Exercise 3.8
    val s = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    // s will be Cons(1, Cons(2, Cons(3, Nil)), and that is the relation

    // Exercise 3.9
    def length[A](list: List[A]) = foldRight(list, 0)((x, y) => y + 1)

    // Exercise 3.10 , .\tail recursive version of the above foldRight
    // foldLeft, means the operation is mostly folding it from left to right
    def foldLeft[A, B](list: List[A], init: B)(f: (B, A) => B): B = {
      @annotation.tailrec
      def go(innerList: List[A], acc: B): B =
        innerList match {
          case Nil => acc
          case Cons(x, xs) => go(xs, f(acc, x))
        }

      go(list, init)
    }

    // We easily arrived at the above foldLeft using the below steps
    // 1st step
    def sumForInt[A, B](list: List[Int]) = {
      def go(innerList: List[Int], acc: Int): Int = innerList match {
        case Cons(x, xs) => go(xs, acc + x)
        case Nil => acc
      }

      go(list, 0)
    }

    //2nd step
    def anythingForInt(list: List[Int], defaultValue: Int, someOperation: (Int, Int) => Int) = {
      def go(innerList: List[Int], acc: Int): Int = innerList match {
        case Cons(x, xs) => go(xs, someOperation(acc, x))
        case Nil => acc
      }
      go (list, defaultValue)
    }

    //3rd step
    def anyOpOnAnything[A, B](list: List[A], defaultValue: B, f:(B, A) => B): B = {
      def go(innerList: List[A], acc: B): B = innerList match {
        case Cons(x, xs) => go (xs, f(acc, x))
        case Nil         => acc
      }

      go(list, defaultValue)
    }

    // 4th step - just renaming the above function as fold
    def fold[A, B](list: List[A], defaultValue: B, f:(B, A) => B): B = {
      def go(innerList: List[A], acc: B): B = innerList match {
        case Cons(x, xs) => go (xs, f(acc, x))
        case Nil         => acc
      }

      go(list, defaultValue)
    }

    // 5th why the above is foldLeft is explained when you do an append operation
    // Now you dont end up having a reversed list...starting from right folding towards left.
    def appendUsingFold[A](leftList: List[A], rightList: List[A]): List[A] =
      fold(leftList, rightList, (acc: List[A], eachElement: A) => Cons(eachElement, acc))


    // Exercises 3.11
    def sumFoldLeft(list: List[Int]): Int = foldLeft(list, 0)(_ + _)

    def productFoldLeft(list: List[Int]): Int = foldLeft(list, 1)(_ * _)

    def lengthFoldLeft(list: List[Int]): Int = foldLeft(list, 0)((a, b) => a + 1)

    // Exercise 3.12
    def reverseList[A](list: List[A]): List[A] = foldLeft(list, List[A]())((a, b) => Cons(b, a))

    def append1(a: List[Int], b: List[Int]): List[Int] = {
      a match {
        case Nil => b
        case Cons(x, xs) => Cons(x, append1(xs, b))
      }
    }

    //Exercise 3.13
    // Hard: Can you write foldLeft in terms of foldRight?
    // How about the other way around?
    // Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively,
    // which means it works even for large lists without overflowing the stack.
    // I used reverse string, but basically things work if you change the order of arguments
    def foldLeftViaFoldRightBetterWay[A, B](list: List[A], init: B)(f: (A, B) => B) =
      foldRight(reverseList(list), init)((a, b) => f(a, b))

    def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverseList(as), z)((a,b) => f(b, a))

    // Exercise 3.14
    // This is really good, that you need to think twice your foldright implementation is actually folding it right
    def appendWithFoldRight[A](list: List[A], list1: List[A]): List[A] =
    foldRight(list, list1)((a: A, b: List[A]) => Cons(a, b))

    // This is really good, that you need to think twice that your foldLeft implementatio is actually folding it left
    def appendWithFoldLeft[A](list: List[A], list1: List[A]): List[A] =
    foldLeft(reverseList(list), list1)((a: List[A], b: A) => Cons(b, a))

    //Exercise 3.15
    // Hard: Write a function that concatenates a list of
    // lists into a single list. Its runtime should be linear in the
    // total length of all lists. Try to use functions we have already defined.
    def concantenateListOfList[A](list: List[List[A]]): List[A] = {
      def go(innerList: List[List[A]], acc: List[A]): List[A] = innerList match {
        case Cons(x, tail) => x match {
          case Cons(h, t) => go(tail, appendWithFoldLeft(acc, Cons(h, t)))
          case Nil => go(tail, acc)
        }
        case Nil => acc
      }

      go(list, Nil)
    }

    // Exercise 3.16
    // Write a function that transforms a list of integers by adding
    // 1 to each element. (Reminder: this should be a pure function that returns a new List!)
    def add1ToEachElement(list: List[Int]): List[Int] =
    foldRight(list, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

    // Exercise 3.17
    // Write a function that turns each value in a List[Double] into a String.
    // You can use the expression d.toString to convert some d: Double to a String.
    def convertEachDoubleToString(list: List[Double]): List[String] =
    foldRightViaFoldLeft (list, Nil: List[String])((a, acc) => Cons(a.toString, acc))

    // Exercise 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] = {
      foldRightViaFoldLeft(as, Nil: List[B])((a, acc) => Cons(f(a), acc))
    }

    // Exercise 3.19
    // Write a function filter that removes elements from a list unless
    // they satisfy a given predicate. Use it to remove all odd numbers from a List[Int].

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {

      def go( innerList: List[A], acc: List[A]): List[A] =
      innerList match {
        case Nil => acc
        case Cons(x, xs) if f(x) => go (xs, Cons(x, acc))
        case Cons(x, xs) => go(xs, acc)
      }

      go(as, Nil)
    }
  }

  // TODO: Exercise from  3.20 to 3.24; these are same patterns and hence it is time to move forward

  // NOTE. 3.4.2. Loss of efficiency when assembling list functions from simpler components
  // One of the problems with List is that, although we can often express operations and algorithms in terms of
  // very general-purpose functions, the resulting implementation isn’t always efficient—we may end up making
  // multiple passes over the same input, or else have to write explicit recursive loops to allow early termination.
}
