package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad

/**
 * The point is that programming with a composable abstraction like List is much nicer
 * than programming directly with the primitive I/O operations. Lists aren’t really special
 * in this regard—they’re just one instance of a composable API that’s pleasant to use.
 * And we shouldn’t have to give up all the nice compositionality that we’ve come to
 * expect from FP just to write programs that make use of efficient, streaming I/O. Luckily
 * we don’t have to. If we like the metaphor of lists or
 * streams, we can design a list-like API for expressing I/O computations. If we discover
 * some other composable abstraction, we
 */
object StreamingIO {

  /**
   * Let us start with a stream transducer, as the first step of forming our nice level of
   * abstraction, that can smash streams into IO, considering avoiding resource leaks,
   * less memory and performance.
   *
   * A stream transducer specifies a transformation from one stream to another.
   * Let’s consider a simple data type, Process, that lets us
   * express stream transformations.
   * @tparam I
   * @tparam O
   */
  sealed trait Process[I, O] {
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs      => recv(None)(xs)
      }
      case Emit(h, t) => h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i    => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }

      go(this)
    }

    /**
     * Exercise 15.5
     * READ THIS, after you read rest of this source file until Exercise 15.4
     * We can build up more complex stream transformations by composing Process values.
     * Given two Process values f and g, we can feed the output of f to the input of g. We’ll
     * name this operation |> (pronounced pipe or compose) and implement it as a function
     * on Process.
     * It has the nice property that f |> g fuses the transformations done by f
     * and g. As soon as values are emitted by f, they’re transformed by g.
     * Hard: Implement |> as a method on Process. Let the types guide your implementation.
     */
    def |>[O2](p2: Process[O, O2]): Process[I, O2] = {
      p2 match {
        case Halt()     => Halt()
        case Emit(x, t) => Emit(x, this |> t)
        case Await(f) => this match {
          case Emit(h, t) => t |> f(Some(h))
          case Halt()     => Halt() |> f(None)
          case Await(g)   => Await((i: Option[I]) => g(i) |> p2)
        }
      }
    }

    /**
     * READ THIS ONLY IF YOU HAVE COMPLETED UNTIL Exercise 15.5
     * We can now write expressions like filter(_ % 2 == 0) |> lift(_ + 1) to filter and map
     * in a single transformation. We’ll sometimes call a sequence of transformations like
     * this a pipeline.
     * Since we have Process composition, and we can lift any function into a Process,
     * we can easily implement map to transform the output of a Process with a function:
     * This means that the type constructor Process[I,_] is a functor
     */
    def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt()      => p
      case Emit(h, t)  => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt()      => Halt()
      case Emit(h, t)  => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    }

  }

  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

  case class Halt[I, O]() extends Process[I, O]

  /**
   * A Process[I,O] can be used to transform a stream containing I values to a stream of
   * O values. But Process[I,O] isn’t a typical function Stream[I] => Stream[O], which
   * could consume the input stream and construct the output stream. Instead, we have a
   * state machine that must be driven forward with a driver. a function that simultaneously
   * consumes both our Process and the input stream. A Process can be in one of three
   * states, each of which signals something to the driver.\
   *
   * 1) Emit(head,tail) indicates to the driver that the head value should be emitted
   * to the output stream, and the machine should then be transitioned to the tail
   * state.
   *
   * 2) Await(recv) requests a value from the input stream. The driver should pass
   * the next available value to the recv function, or None if the input has no more
   * elements
   *
   * 3) Halt indicates to the driver that no more elements should be read from the
   * input or emitted to the output
   *
   * Thus, given p: Process[I,O] and in: Stream[I], the expression p(in) produces a
   * Stream[O]. What’s interesting is that Process is agnostic to how it’s fed input.
   */

  /**
   * We can convert any function f: I => O to a Process[I,O]. We just Await and then
   * Emit the value received, transformed by f:
   */
  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None    => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  /**
   * Since the repeat combinator recurses forever and Emit is strict in its arguments, we
   * have to be careful not to use it with a Process that never waits! For example, we can’t
   * just say Emit(1).repeat to get an infinite stream that keeps emitting 1. Remember,
   * Process is a stream transducer, so if we want to do something like that, we need to
   * transduce one infinite stream to another:
   * @return
   */
  def units: Stream[Unit] = Stream.continually(())
  def process: Process[Unit, Int] = lift((_: Unit) => 1)
  def k: Stream[Int] = process(units)

  /**
   * We can do more than map the elements of a stream from one type to another—we
   * can also insert or remove elements. Here’s a Process that filters out elements that
   * don’t match the predicate p:
   */
  def filter[I](f: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if f(i) => Emit[I, I](i)
      case _               => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await[Double, Double] {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None    => Halt()
      }
    go(0.0)
  }

  val even = filter((x: Int) => x % 2 == 0)
  val evens = even(Stream(1, 2, 3, 4)).toList

  /**
   * EXERCISE 15.1
   */
  def take[I](n: Int): Process[I, I] = {
    def go(t: Int): Process[I, I] = {
      Await[I, I] {
        case Some(i) if t < n => Emit(i, go(t + 1))
        case _                => Halt()
      }
    }
    go(0)
  }

  def drop[I](n: Int): Process[I, I] = {
    def go(t: Int): Process[I, I] = {
      Await[I, I] {
        case _ if t <= n => go(t + 1)
        case Some(i)     => Emit(i, go(t + 1))
        case None        => Halt()
      }
    }
    go(1)
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] = {
    Await[I, I] {
      case Some(i) if f(i) => Emit[I, I](i)
      case _               => Halt()
    }.repeat
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    Await[I, I] {
      case Some(i) if !f(i) => Emit[I, I](i)
      case _                => Halt()
    }.repeat
  }

  // EXERCISE 15.2
  /**
   * Implement count. It should emit the number of elements seen so far. For instance,
   * count(Stream("a", "b", "c")) should yield Stream(1, 2, 3) (or Stream(0, 1, 2, 3),
   * your choice).
   */
  def count[I]: Process[I, Int] = {
    def go(acc: Int): Process[I, Int] = {
      Await[I, Int] {
        case Some(i) => Emit(acc + 1, go(acc + 1))
        case _       => Halt()
      }
    }
    go(0)
  }

  /**
   * EXERCISE 15.3
   * Implement mean. It should emit a running average of the values seen so far.
   */
  def mean: Process[Double, Double] = {
    def go(n: Int, sum: Double): Process[Double, Double] = {
      Await[Double, Double] {
        case Some(i) => Emit((sum + i) / n, go(n + 1, sum + i))
        case _       => Halt()
      }
    }
    go(1, 0)
  }

  /**
   * The functions sum, count and mean all share a common pattern. Each has a
   * single piece of state, has a state transition function that updates this state in response to
   * input, and produces a single output. We can generalize this to a combinator, loop.
   * It seems there is a typo in Red book again. The awesome brain of the author drained out
   * by the last chapter trying to make us understand things!
   */

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    Await {
      case Some(i) => f(i, z) match {
        case (o, s2) => Emit[I, O](o, loop(s2)(f))
      }
      case _ => Halt()
    }

  // Exercise 15.4
  // Write sum and count in terms of loop.
  def sumLoop: Process[Double, Double] =
    loop(0.0)({ case (i, acc) => (acc + i, acc + i) })

  def countLoop[I]: Process[I, Int] =
    loop(0)({ case (_, acc) => (acc + 1, acc + 1) })

  // Continue reading the below stuff if you have read and understand Exercise 15.5, and the rest of the
  // explanations.
  /**
   * This Monad instance is the same idea as the Monad for List. What makes Process
   * more interesting than just List is that it can accept input. And it can transform that
   * input through mapping, filtering, folding, grouping, and so on. It turns out that
   * Process can express almost any stream transformation, all the while remaining agnostic
   * to how exactly it’s obtaining its input or what should happen with its output.
   * @tparam I
   * @return
   */
  def monad[I]: Monad[({ type f[x] = Process[I, x] })#f] =
    new Monad[({ type f[x] = Process[I, x] })#f] {
      def unit[O](o: => O): Process[I, O] = Emit(o)

      def flatMap[O, O2](p: Process[I, O])(
        f: O => Process[I, O2]
      ): Process[I, O2] =
        p flatMap f
    }

}
