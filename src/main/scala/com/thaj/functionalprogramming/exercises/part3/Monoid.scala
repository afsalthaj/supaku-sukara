package com.thaj.functionalprogramming.exercises.part3

import com.thaj.functionalprogramming.example.exercises.{Branch, Leaf, Tree}
import com.thaj.functionalprogramming.example.exercises.part2.Par.Par
import com.thaj.functionalprogramming.example.exercises.part2.{Par, Prop, Gen}

import scala.collection.immutable.Stream.{Empty, cons}

/**
 * Created by afsalthaj on 25/02/17.
 *
 * This chapter covers the basics of Monoid:
 * The name monoid comes from mathematics. In category theory, it means a category with one object.
 * We’ll see how monoids are useful in two ways: they facilitate parallel computation by giving us the
 * freedom to break our problem into chunks that can be computed in parallel;
 * and they can be composed to assemble complex calculations from simpler pieces.
 *
 * Focus on any algebra, that has an identity operation and associative in nature
 *
 * Ex:
 *
 * String concatenation: "" and it is associative
 * Multiplication: 1 (which “does nothing” when multiplied to another integer) and it is associative
 * Addition: 0 (which “does nothing” when added to another integer)and it is associative
 * && : true and it is associative
 * || : false and it is associative
 *
 * A monoid consists of the following:
 * Some type A
 * An associative binary operation, op, that takes two values of type A and combines them
 * into one: op(op(x,y), z) == op(x, op(y,z)) for any choice of x: A, y: A, z: A
 * A value, zero: A, that is an identity for that operation: op(x, zero) == x and op(zero, x) == x for any x: A
 *
 */

object Monoid {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid= new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  val listMonoid = new Monoid[List[String]] {
    def op(a1: List[String], a2: List[String]) = a1 ++ a2
    val zero = Nil
  }

  /**
   * Exercise 10.1
   * Give Monoid instances for integer addition and multiplication as well as the Boolean operators.
   * val intAddition: Monoid[Int]
   * val intMultiplication: Monoid[Int]
   * val booleanOr: Monoid[Boolean]
   * val booleanAnd: Monoid[Boolean]
   */

  val intAddition = new Monoid[Int]{
    def op(a1: Int, b1: Int): Int = a1 + b1
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, b1: Int) = a1 * b1
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, b1: Boolean): Boolean = a1 || b1
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, b1: Boolean): Boolean = a1 && b1
    val zero = true
  }

  /**
   * Exercise 10.2
   * Give a Monoid instance for combining Option values.
   * def optionMonoid[A]: Monoid[Option[A]]
   */

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], b1: Option[A]) = a1.orElse(b1)
    val zero = None
  }

  // Exercise 10.3
  // A function having the same argument and return type
  // is sometimes called an endofunction.[2] Write a monoid for endofunctions.
  // The Greek prefix endo- means within, in the sense that an endofunction’s codomain is within its domain.

  def endoMonoid[A]: Monoid[A => A] = new Monoid[ A => A ]{
    def op (a: A => A, b: A => A): A => A = a compose b
    val zero = (a: A) => a
  }

  // Exercise 10.4
  // Use the property-based testing framework we developed in part 2 to implement a
  // property for the monoid laws. Use your property to test the monoids we’ve written.
  // def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val p: Gen[(A, A, A)] = gen.flatMap( b => gen.flatMap(c => gen.map(d => (b, c, d))))

    Prop.forAll(p){case (x, y, z) =>
      m.op(m.op(x,y), z) == m.op(x, m.op(y,z)) && m.op(x, m.zero) == x && m.op(m.zero, x) == x
    }
  }

  /**
   * There is a slight terminology mismatch between programmers and
   * mathematicians when they talk about a type being a monoid versus having a
   * monoid instance. As a programmer, it’s tempting to think of the instance of type Monoid[A]
   * as being a monoid. But that’s not accurate terminology. The monoid is actually both things—the type
   * together with the instance satisfying the laws. It’s more accurate to say that the type A FORMS a
   * monoid under the operations defined by the Monoid[A] instance. Less precisely, we might say that “type A is a monoid,”
   * or even that “type A is monoidal.” In any case, the Monoid[A] instance is simply evidence of this fact.
   * It’s simply a type A and an implementation of Monoid[A] that satisfies the laws. Stated tersely, a monoid is a type
   * together with a binary operation (op) over that type,
   * satisfying associativity and having an identity element (zero).
   */

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // We can get the dual of any monoid just by flipping the `op`.
  // Copied from fpinscala
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  /**
   * But what if our list has an element type that doesn’t have a Monoid instance? Well,
   * we can always map over the list to turn it into a type that does:
   * def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B
   */


  // Exercise 10.5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b: B, a: A) => m.op(b, f(a)))
  }


  // Hard: The foldMap function can be implemented using either
  // foldLeft or fold-Right. But you can also write foldLeft and foldRight using foldMap! Try it.
  // Copied from fpinscala
  def foldRight[A, B](a: List[A], init: B)(f: (A, B) => B): B = {
    foldMap(a, endoMonoid[B])(f.curried)(init)
  }

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)


  /**
   * As an example, suppose we have a sequence a, b, c, d that we’d
   * like to reduce using some monoid. Folding to the right, the combination of a, b, c, and d would look like this:
   * op(a, op(b, op(c, d)))
   * Folding to the left would look like this:
   * op(op(op(a, b), c), d)
   * But a balanced fold looks like this:
   * op(op(a, b), op(c, d))
   */
  // Exercise 10.7
  // Implement a foldMap for IndexedSeq.[4] Your implementation should use the strategy of
  // splitting the sequence in two, recursively processing each half, and then adding the answers together with the monoid.
  // 4 Recall that IndexedSeq is the interface for immutable data structures supporting efficient random access.
  // It also has efficient splitAt and length methods.

  def foldMapV[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
     if(as.isEmpty)
       m.zero
       else if(as.size == 1)
       f(as.head)
     else {
       val (l, r) = as.splitAt(as.size/2)
       m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
     }
  }


  // Exercise 10.8
  // HARD: Lifting Monoid[A] to Monoid[Par[A]]
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]{
    def op(a: Par[A], b: Par[A]): Par[A] = Par.map2Fixed(a, b)(m.op)
    val zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parallelMonoid: Monoid[Par[B]] = par(m)
    val parOfListOfB: Par[List[B]] = Par.parMap(v.toList)(f)
      Par.flatMapUsingJoin(parOfListOfB){ listOfB => {
        foldMapV(listOfB.toIndexedSeq, parallelMonoid)(b => Par.lazyUnit(b))
      }
    }
  }

  // Exercise 10.9
  // Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll need
  // to come up with a creative Monoid.
  // The monoid follows the laws: m.op(m.op(x,y), z) == m.op(x, m.op(y,z)) && m.op(x, m.zero) == x && m.op(m.zero, x) == x
  val intMaxMonoid = new Monoid[(Option[Int], Boolean)] {
    def op(a: (Option[Int], Boolean), b: (Option[Int], Boolean)): (Option[Int], Boolean) = (a, b) match {
      case ((s@Some(x), bool1), (None, bool2)) => (s, bool1 && bool2)
      case ((None, bool1), (s@Some(x), bool2)) => (s, bool1 && bool2)
      case ((s1@Some(x), bool1), (s2@Some(y), bool2)) => if (y > x) (s2, bool1 && bool2) else (None, false)
      case ((None, bool1), (None, bool2)) => (None, bool1 && bool2)
    }

    val zero: (Option[Int], Boolean) = (None, true)
  }

  // this solution is significantly different from fpinscala, but it is tested for basic scenarios
  def isOrdered(a: IndexedSeq[Int]) = {
     foldMap(a.toList,intMaxMonoid)(b => (Some(b), true))._2
  }

  // Parallel parsing
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  // Write a monoid instance for WC and make sure that it meets the monoid laws.
  // val wcMonoid: Monoid[WC]
  val monoidWC: Monoid[WC] = new Monoid[WC] {
    def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(x), Stub(y))                   => Stub(x + y)
      case (Stub(x), Part(i, j, k))             => Part(x+i, j, k)
      case (Part(i, j, k), Stub(x))             => Part(i, j, k+x)
      case (Part(i1, j1, k1), Part(i2, j2, k2)) => Part(i1, (j1 + j2) + (if((k1+i2).isEmpty) 0 else 1), k2)
    }

    val zero: WC = Stub("")
  }

  // EXERCISE 10.11
  // Use the WC monoid to implement a function that counts words in a String by recursively
  // splitting it into substrings and counting the words in those substrings.
  // a bit tricky algorithm
  def addWC(a: String): Int = {
    def toWC(a: Char): WC =
      if(a.isWhitespace) Part("", 0, "") else Stub(a.toString)

      foldMapV(a.toList.toIndexedSeq, monoidWC)(toWC) match {
        case Stub(s) => s.length min 1
        case Part(x, y, z) => y + x.length min 1 + z.length min 1
      }
  }

  /**
   * ints.foldRight(0)(_ + _)
   * Looking at just this code snippet, we shouldn’t have to care about the type of ints. It
   * could be a Vector, a Stream, or a List, or anything at all with a foldRight method.
   * We can capture this commonality in a trait:
   */
  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    // EXERCISE 10.15
    // Any Foldable structure can be turned into a List. Write this conversion in a generic way
    def toList[A](fa: F[A]): List[A] = foldRight(fa)(Nil: List[A])(_ :: _)

  }

  // EXERCISE 10.12
  /**
   * Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
   * Remember that foldRight, foldLeft, and foldMap can all be implemented in terms
   * of each other, but that might not be the most efficient implementation.
   * Some kind of learning experiments in this implementation
   */
  object FoldableList extends Foldable[List]{
    def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B = {
      as match  {
        case Nil => z
        case x :: xs => f(x, foldRight(xs)(z)(f))
      }
    }

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case x :: xs => foldLeft(xs)(f(z, x))(f)
      }
    }

    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((acc, a) => mb.op(acc, f(a)))
  }

  object FoldableStream extends Foldable[Stream]{
    def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B = {
      as match  {
        case Empty => z
        case cons(h, t) => f(h, foldRight(t)(z)(f))
      }
    }

    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Empty => z
        case cons(h, t) => foldLeft(t)(f(z, h))(f)
      }
    }

    def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((acc, a) => mb.op(acc, f(a)))
  }

  object FoldableIndexedSeq extends Foldable[IndexedSeq]{
    def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A,B) => B): B = {
     as.foldRight(z)(f)
    }

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = {
      as.foldLeft(z)(f)
    }

    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
     as.foldLeft(mb.zero)((acc, a) => mb.op(acc, f(a)))
  }

  // EXERCISE 10.13
  // Recall the binary Tree data type from chapter 3. Implement a Foldable instance for it.
  val foldableTree = new Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
      as match  {
        case Leaf(x) => f(x, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }
    }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Leaf(x) => f(z, x)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }
    }

    /**
     * {{{
     *   scala> Branch(Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))), Leaf(5))
     *   res10: com.thaj.functionalprogramming.example.exercises.Branch[Int] =
     *     Branch(Branch(Leaf(2),Branch(Leaf(3),Branch(Leaf(4),Leaf(5)))),Leaf(5))
     *   scala> foldableTree.foldMap(res8)(_.toInt)(intAddition)
     *   res11: Int = 19
     * }}}
     */
    def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((acc, a) => mb.op(acc, f(a)))
  }

  // EXERCISE 10.14
  val foldableOption = new Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = {
      as match  {
        case Some(x) => f(x, z)
        case None => z
      }
    }

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Some(x) => f(z, x)
        case None => z
      }
    }

    def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((acc, a) => mb.op(acc, f(a)))
  }

  // 10.6 Composing Monoids
  // Exercise 10.16
  // Prove it. Notice that your implementation of op is obviously associative so long as A.op
  // and B.op are both associative.
  /**
   * scala> def op(a: Int, b: Int) = a + b
   * op: (a: Int, b: Int)Int
   *
   * scala> def op1(a: String, b: String) = a + b
   * op1: (a: String, b: String)String
   *
   * scala> op1("1",(op1("1","2")))
   * res6: String = 112
   *
   * scala> op1(op1("1","1"),"2")
   * res7: String = 112
   *
   * scala> def op2(a: (Int, String), b: (Int, String)): (Int, String) = op(a._1, b._1) -> op1(a._2, b._2)
   * op2: (a: (Int, String), b: (Int, String))(Int, String)
   *
   * scala> op2((1,"2"), (3,"4"))
   * res8: (Int, String) = (4,24)
   *
   * scala> op2((1,"2"), op2((3,"4"), (4, "5")))
   * res9: (Int, String) = (8,245)
   *
   * So if A and B are Monoid, then Tuple(A, B) can also form a Monoid
   */
  def productMonoid[A,B](A : Monoid[A], B: Monoid[B]): Monoid[(A,B)] = {
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) =  A.op(x._1, y._1) -> B.op(x._2, y._2)
      val zero: (A, B) = (A.zero, B.zero)
    }
  }

  /**
   * Some data structures form interesting monoids as long as the types of the elements
   * they contain also form monoids. For instance, there’s a monoid for merging key-value
   * Maps, as long as the value type is a monoid.
   *
   * So, V is a monoid, then Map[K, V] can also form a monoid.
   */
  def  mapMergeMonoid[K,V](v: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    val zero: Map[K, V] = Map[K, V]()
    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
      (a.keySet ++ b.keySet).foldLeft(zero)((acc, k) => {
        acc.updated(k, v.op(a.getOrElse(k, v.zero), b.getOrElse(k, v.zero)))
      })
    }
  }

  /**
   * scala> val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
   * m1: Map[String,Map[String,Int]] = Map(o1 -> Map(i1 -> 1, i2 -> 2))
   * scala> val m2 = Map("o1" -> Map("i2" -> 3))
   * m2: Map[String,Map[String,Int]] = Map(o1 -> Map(i2 -> 3))
   * scala> val m3 = M.op(m1, m2)
   * m3: Map[String,Map[String,Int]] = Map(o1 -> Map(i1 -> 1, i2 -> 5))
   */
   val M:  Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid[String, Int](intAddition))

   // EXERCISE 10.17
   // Write a monoid instance for functions whose results are monoids.
   def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = {
     new Monoid[A => B] {
       def op(f1: A => B, f2: A => B): A => B = (a: A) => B.op(f1(a), f2(a))
       val zero: A => B = (a: A) => B.zero
     }
   }

  /**
   * EXERCISE 10.18
   * A bag is like a set, except that it’s represented by a map that contains one entry per
   * element with that element as the key, and the value under that key is the number of
   * times the element appears in the bag. For example:
   * scala> bag(Vector("a", "rose", "is", "a", "rose"))
   * res0: Map[String,Int] = Map(a -> 2, rose -> 2, is -> 1)
   * Use monoids to compute a “bag” from an IndexedSeq
   */
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMap(as.toList, mapMergeMonoid[A, Int](intAddition))(a => Map[A, Int](a -> 1))
}
