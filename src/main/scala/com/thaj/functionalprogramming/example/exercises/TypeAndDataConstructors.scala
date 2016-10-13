package main.scala.com.thaj.functionalprogramming.example.exercises

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
        if (inc >=  n)  a
        else
          a match {
            case Nil => Nil
            case Cons(_, tail) => go(tail, inc+1)
          }
      }

      go(l, 0)
    }

    // An awesome higher order functions! Believe me, it is awesome!
    // Seems like I am copying data :(
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      def go (a: List[A]): List[A] = {
        a match {
          case Nil => Nil
          case Cons(h, t1) if f(h) => go(t1)
          case Cons(h, tail) => Cons(h, go(tail))
        }
      }
      go(l)
    }

    // appending without any copy operations
    // Note that this definition only copies values
    // until the first list is exhausted, so its
    // runtime and memory usage are determined only by the length of a1.
    // \The remaining list then just points to a2. If we were to implement this same
    // function for two arrays, we’d be forced to copy all the elements in both arrays into the result.
    // In this case, the immutable linked list is much more efficient than an array!

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

  }
}