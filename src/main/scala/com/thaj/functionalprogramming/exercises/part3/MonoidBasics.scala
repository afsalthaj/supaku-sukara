package com.thaj.functionalprogramming.exercises.part3

import com.thaj.functionalprogramming.example.exercises.part2.Par.Par
import com.thaj.functionalprogramming.example.exercises.part2.{Par, Prop, Gen}

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

object MonoidBasics {
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
    def op(a1: Int, b1: Int) = a1 + b1
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
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

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
  val intMaxMonoid = new Monoid[(Int, Boolean)] {
    def op(a: (Int, Boolean), b: (Int, Boolean)): (Int, Boolean) = if(b._1 >= a._1) (b._1, true) else (a._1, false)
    val zero: (Int, Boolean) = (0, true)
  }

  // this solution is significantly different from fpinscala, but it is tested for basic scenarios
  def isIndexedSeqAOrdered(a: IndexedSeq[Int]) = {
     foldMap(a.toList,intMaxMonoid)(b => (b, true))._2
  }
}