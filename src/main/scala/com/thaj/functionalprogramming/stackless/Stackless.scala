package com.thaj.functionalprogramming.stackless

/**
 * Created by afsalthaj on 8/13/17.
 */
object Stackless {

  // Read http://blog.higher-order.com/assets/trampolines.pdf
  // Before you read further, the above pdf is a must read.
  sealed trait Trampoline[+A] {
    // this is only for demonstration purpose that the final
    // runT call here is a "Jump" from the compiler's perspective
    // and not any sort of recursion that builds up the compiler's stack.
    /**
     *
     * {{{
     *     def fetchIfDone: A = this match {
     * case Done(k) => k
     * }
     * *
     * final def runT: A = {
     * var cond = true
     * var result: Trampoline[A] = this
     * while (cond) {
     * result match {
     * case More(k) => result = k();
     * case Done(_) => cond = false
     * }
     * }
     * result.fetchIfDone
     * }}}
     *
     * /**
     *
     * Ok.. What does this mean? Once your description is created, where building
     * up description didn't involve any sort of run or didn't fill up the stack
     * because each step yields a trampoline and it stops there. The final interpretation
     * is done by the above run method. You must be wondering why the function
     * consist of `var` and mutations. As mentioned, this is a demonstration purpose.
     * It shows that the final runT method doesn't involve any filling up stack with
     * any frames to remember any state of any step. You would easily guess that
     * the above ugly code can be replaced with the below tail recursive call which
     * is elegant - and yes! that's exactly the same.
     * {{{
     *   final def runT : A =
     *     this match {
     *       case More (k) => k (). runT
     *       case Done (v) => v
     *     }
     * }}}
     * *
     * The point here is, your description doesn't take up
     * any stack, and so is the interpreter too. This is the basics of trampoline's
     * interpreter.
     * */
     *
     * @return
     */

    final def resume: Either[() => Trampoline[A], A] = this match {
      case Done(v) => Right(v)
      case More(k) => Left(k)
      case FlatMap(a, f) => a match {
        case Done(v)       => f(v).resume
        case More(k)       => Left(() => FlatMap(k(), f))
        case FlatMap(b, g) => (FlatMap(b, (x: Any) => FlatMap(g(x), f)): Trampoline[A]).resume
      }
    }

    final def runT: A = resume match {
      case Right(v) => v
      case Left(k)  => k().runT
    }
  }

  /**
   * Trying to define a flatMap in trampoline to solve the state problem
   * {{{
   *   def flatMap[B](f: A => Trampoline[B]) = More[B](() => f(runT))
   * }}}
   */

  case class More[+A](k: () => Trampoline[A])
    extends Trampoline[A]

  case class Done[+A](result: A)
    extends Trampoline[A]

  def even[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil     => Done(true)
      case x :: xs => More(() => odd(xs))
    }

  def odd[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil     => Done(false)
      case x :: xs => More(() => even(xs))
    }

  def main(args: Array[String]): Unit = {
    val description = even((1 to 100001).toList)

    /**
     * instead of recursing directly the functions now
     * return as Trampoline which can be executed tail-recursively
     * by calling the runT method.
     */
    println(description.runT)
  }

  /**
   * applying trampoline with the state
   * The wrong one
   * {{{
   *     case class State[S, A] (runS: S => Trampoline[(A, S)]){
   *       def flatMap[B](f: A => State[B, S]): State[B, S] = State(s => {
   *         More(() => {
   *           val (a, s1) = runS(s).runT
   *           More(() => f(a).runS(s1))
   *           })
   *         })
   *     }
   * }}}
   * But that turns out to be insufficient. The zipIndex example
   * from section 1 will still overflow the stack for large lists,
   * this time for even smaller lists. The problem is that the call
   * to runT is not in the tail position, so it can’t be optimized or
   * wrapped in a Trampoline.
   */

  // Please refer to the paper again, where state engine is used to index all
  // the numbers in a list was affected by Stack overflow. Ok? now what is the solution
  // in general terms. We must be able to build a description without non-tail calls on
  // runT, and then finally call the non-dangerous runT function to get your final result.

  /**
   * Final way out. A trampoline of this form can be thought of as a call to a
   * subroutine sub whose result is returned to the continuation
   * k.
   */

  case class FlatMap[A, +B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]

}