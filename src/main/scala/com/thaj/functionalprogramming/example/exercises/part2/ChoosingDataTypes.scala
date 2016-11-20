package com.thaj.functionalprogramming.example.exercises.part2

object DesignAspects {
  /**
   * When you begin designing a functional library, you usually have some general ideas
   * about what you want to be able to do, and the difficulty in the design process is in
   * refining these ideas and finding a data type that enables the functionality you want. In
   * our case, we’d like to be able to “create parallel computations,” but what does that
   * mean exactly? Let’s try to refine this into something we can implement by examining
   * a simple, parallelizable comp
   */
  def sum(ints: Seq[Int]) = ints.foldLeft(0)(_ + _)

  // Instead of folding sequentially, you could go for divide and conquer algorithm
  // IndexedSeq is a super-class of random access sequences like Vector in the standard
  // library. Unlike lists these sequences provide an efficient splitAt method for dividing
  // them into two parts at a particular index.

  def sumParallel(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1) {
      ints.headOption.getOrElse(0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      // Recursively sums both halves and adds the results together
      // however still it is partly sequential...However this is not what we are
      // trying to solve. We are trying to come up with a data type that represents
      // a parallel computation and the companion object functions.
      /* scala> sumParallel(Vector(1,2,3,4))
         Vector(1, 2)+Vector(3, 4)
         Vector(1)+Vector(2)
         // the primary second half, which is (3, 4) is handled later on
         Vector(3)+Vector(4)
         res25: Int = 10
        */
      sumParallel(l) + sumParallel(r)
    }

  /*
    Look at the line sum(l) + sum(r), which invokes sum on the two halves recursively. Just
    from looking at this single line, we can see that any data type we might choose to represent
    our parallel computations needs to be able to contain a result. That result will
    have some meaningful type (in this case Int), and we require some way of extracting
    this result. Let’s apply this newfound knowledge to our design. For now, we can just
    invent a container type for our result, Par[A] (for parallel), and legislate the existence
    of the functions we need:
   */

  // so, we have defined a new data type that represents a parallel computation
  /**
   * {{{ }}}, for taking an unevaluated A and returning a
   * computation that might evaluate it in a separate thread. We call it unit because
   * in a sense it creates a unit of parallelism that just wraps a single value
   */
  // def unit[A](a: => A): Par[A], for extracting the resulting value from a parallel
  //computation
  sealed trait Par[A]
  case class ParA[A](a: () => A) extends Par[A]

  object Par {
    def unit[A](a: => A): Par[A] = ParA(() => a)
  }
  /*
  {{{
   def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.length <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val leftParallelSum = Par.unit(sum(l))
      val rightParallelSum = Par.unit(sum(r))

      Par.get(leftParallelSum) + Par.get(rightParallelSum)

    }
  }
  }}}
  We’ve wrapped the two recursive calls to sum in calls to unit, and we’re calling get
  to extract the two results from the two subcomputations.
  }
  */

  /*
  Function arguments in Scala are strictly evaluated from left to right, so if unit delays execution until get is
  called, we will both spawn the parallel computation and wait for it to finish before spawning the second parallel
  computation. This means the computation is effectively sequential!
  If unit starts evaluating its argument right away, the next thing to happen is that get
  will wait for that evaluation to complete. So the two sides of the + sign won’t run in
  parallel if we simply inline the sumL and sumR variables. We can see that unit has a definite
  side effect, but only with regard to get. That is, unit simply returns a Par[Int] in
  this case, representing an asynchronous computation. But as soon as we pass that Par
  to get, we explicitly wait for it, exposing the side effect. So it seems that we want to
  avoid calling get, or at least delay calling it until the very end. We want to be able to
  combine asynchronous computations without waiting for them to finish.
  */
}