package com.thaj.functionalprogramming.example.exercises.part2

import java.util.concurrent.{ ExecutorService, Future }
import java.util.concurrent.TimeUnit

import java.util.concurrent.Callable
/*
  {{{

    // Java ExecutorService and Future

    class ExecutorService {
     def submit[A](a: Callable[A]): Future[A]
    }
    trait Callable[A] { def call: A }

    trait Future[A] {
      def get: A
      def get(timeout: Long, unit: TimeUnit): A
      def cancel(evenIfRunning: Boolean): Boolean
      def isDone: Boolean
      def isCancelled: Boolean
    }

  }}}

  // Our implementation of Par, unit, fork etc

  type Par[A] = ExecutorService => Future[A]
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

 */


object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def isCancelled = false
    def cancel(evenIfRunning: Boolean) = false
    def get(timeout: Long, unit: TimeUnit) = get
  }


  /*
    map2 doesn’t evaluate the call to f in a separate logical thread,
    in accord with our design choice of having fork be the sole function in the API for controlling parallelism.
    We can always do fork(map2(a,b)(f)) if we want the evaluation of f to occur in a separate thread.
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  /**
    This implementation of map2 does not respect timeouts. It simply passes the ExecutorService
    on to both Par values, waits for the results of the Futures af and bf, applies f to them, and
    wraps them in a UnitFuture. In order to respect timeouts, we’d need a new Future
    implementation that records the amount of time spent evaluating af,
    and then subtracts that time from the available time allocated for evaluating bf.
    */

   /**
     * This is the simplest and most natural implementation of fork,but there are some problems
     with it—for one, the outer Callable will block waiting for the “inner” task to complete.
     Since this blocking occupies a thread in our thread pool, or whatever resource backs the ExecutorService,
     this implies that we’re losing out on some potential parallelism. Essentially,
     we’re using two threads when one should suffice. This is a symptom of a more serious problem with
     the implementation that we’ll discuss later in the chapter.
     *
     */
   def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A]{
     def call = a(es).get
   })

   def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // Exercise 7.3
    // Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.
   def map2Fixed[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
     (es: ExecutorService) => {
       val af = a(es)
       val bf = b(es)
        new Future[C]{
          def isDone = af.isDone() && bf.isDone()
          def isCancelled = af.isCancelled() || bf.isCancelled()
          def get = f(af.get(), bf.get())
          def get(timeOut: Long, timeUnit: TimeUnit) = {
            val at1 = System.nanoTime
            val aValue = af.get
            val at2 = System.nanoTime
            val bValue = bf.get(timeOut - (at2 - at1), timeUnit)
            f(aValue, bValue)
          }
          def cancel(evenIfRunning: Boolean): Boolean =
            af.cancel(true) || bf.cancel(true)
        }
     }
   }

   // Exercise 7.4
   // This API already enables a rich set of operations. Here’s a simple example: using lazyUnit,
   // write a function to convert any function A => B to one that evaluates its result asynchronously.
   def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

   /*
     Par[List[Int]] representing a parallel computation that pro- duces a List[Int],
     and we’d like to convert this to a Par[List[Int]] whose result is sorted:
   */
   def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
    map2Fixed(parList, unit(()))((a, _) => a.sorted)
   }

   /* Lift a function. We can lift any function of type A => B to Par[A] => Par[B]
      we can map any function over a Par:
    */
   def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  /** One mistake that we did was, we did sortPar using map2. sortPar is in fact
    * a problem of map
    */
   def sortParProper(a: Par[List[Int]]): Par[List[Int]] = map(a)(_.sorted)

   // here we could see that map is implemented using map2. Both map and map2
   // are being primitive functions, the latter is more powerful. Hence providing
   // a bogus value unit(()) can't be cheating.

   def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
     val list: List[Par[B]] = ps.map(asyncF(_))
     sequence(list)
   }

   // Exercise 7.5
   // Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
   def sequence[A](a: List[Par[A]]): Par[List[A]] =
     list.foldRight(unit(List[A]()): Par[A])(map2(_, _)(_ :: _))
}