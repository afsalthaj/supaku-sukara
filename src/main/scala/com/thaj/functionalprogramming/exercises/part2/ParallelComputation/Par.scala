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

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  /*
    map2 doesn’t evaluate the call to f in a separate logical thread,
    in accord with our design choice of having fork be the sole function in the API for controlling parallelism.
    We can always do fork(map2(a,b)(f)) if we want the evaluation of f to occur in a separate thread.
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) =>
      {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }
  }

  /**
   * This implementation of map2 does not respect timeouts. It simply passes the ExecutorService
   * on to both Par values, waits for the results of the Futures af and bf, applies f to them, and
   * wraps them in a UnitFuture. In order to respect timeouts, we’d need a new Future
   * implementation that records the amount of time spent evaluating af,
   * and then subtracts that time from the available time allocated for evaluating bf.
   */

  /**
   * This is the simplest and most natural implementation of fork,but there are some problems
   * with it—for one, the outer Callable will block waiting for the “inner” task to complete.
   * Since this blocking occupies a thread in our thread pool, or whatever resource backs the ExecutorService,
   * this implies that we’re losing out on some potential parallelism. Essentially,
   * we’re using two threads when one should suffice. This is a symptom of a more serious problem with
   * the implementation that we’ll discuss later in the chapter.
   *
   */

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  // fork(x) == x
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // Exercise 7.3
  // Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.
  /**
   * While map2Fixed fixes the problem of time outs, it is fixing another major problem with
   * with map2. The problem with map2 is that the parallel computations passed into map2 are calling
   * their respective get methods, for the final future to be completely defined. Or in other words,
   * it is taking time to describe future. However, map2 is still valid to be exising as it returns
   * only UnitFuture (a future that has to be executed then and there itself) and not an actual Future (that
   * will execute as only a description). The below function is going to get executed quickly without any wait.
   * Nevertheless, to make sure that the whole map2Fixed is executed in a separate thread pool, it has to be
   * fork(map2Fixed(a, b)(f))
   */
  def map2Fixed[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) =>
      {
        val af = a(es)
        val bf = b(es)
        new Future[C] {
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
  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  /**
   * One mistake that we did was, we did sortPar using map2. sortPar is in fact
   * a problem of map
   */
  def sortParProper(a: Par[List[Int]]): Par[List[Int]] = map(a)(_.sorted)

  // here we could see that map is implemented using map2. Both map and map2
  // are being primitive functions, the latter is more powerful. Hence providing
  // a bogus value unit(()) can't be cheating.
  /**
   * Note that we’ve wrapped our implementation in a
   * call to fork. With this implementa- tion, parMap will return immediately,
   * even for a huge input list. When we later call run, it will fork a single asynchronous
   * computation which itself spawns N parallel com- putations, and then waits for these
   * computations to finish, collecting their results into a list.
   */
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork({
    val list: List[Par[B]] = ps.map(asyncF(f(_)))
    sequence(list)
  })

  // Exercise 7.5
  // Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
  def sequence[A](a: List[Par[A]]): Par[List[A]] =
    a.foldRight(unit(List[A]()): Par[List[A]])(map2(_, _)(_ :: _))

  // Exercise 7.6
  // Implement parFilter, which filters elements of a list in parallel.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    as.map(asyncF(a => (f(a), a))).foldRight(unit(List[A]()))(map2(_, _) {
      case ((true, a), acc)  => a :: acc
      case ((false, a), acc) => acc
    })
  }

  // Exercise 7.7
  //  Hard: Given map(y)(id) == y, it’s a free theorem that map(map(y)(g))(f) ==
  // map(y)(f compose g). (This is sometimes called map fusion, and it can be used as an
  // optimization—rather than spawning a separate parallel computation to compute the
  // second mapping, we can fold it into the first mapping.)13 Can you prove it? You may
  // want to read the paper “Theorems for Free!” (http://mng.bz/Z9f1) to better understand
  // the “trick” of free theore
  // This is not solution to the exercise, but just reasoning the type equality
  /* let y = Par[Y]
     let fun g = Y => G
     let fun f = G => F

     LHS:
     map (map(y)(g))(f) =
     map (map(Par[Y])(Y => G))) f =
     map(Par[G])(G => F)
     = Par[F]

     RHS:
     map (Par[Y]) f compose g =
     map(Par[Y]) (G => F) compose (Y => G)
     map(Par(Y))(Y => G) andThen (G => F)
     map(Par[Y])(Y => F)
     = Par[F]
  */

  /**
   *  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))
   *  map(unit(x))(id) == unit(id(x))
   *  map(unit(x))(id) = unit(x)
   *  map(y)(id) == y
   */

  // map(map(y)(g))(f) == map(y)(f compose g)
  // let g = id
  // map (map(y)(id))(f) = map (y) (f)
  // map (y)(f) = map(y)(f)
  /**
   * Issue with fork (<reference from fpinscala>)
   * Assume that ExecutorService represents a thread pool of size X.
   * Now if fork(x) will execute as far as X != 1; But if it fork(fork(x)). It now constitutes
   * 2 thread pools. Now if fork(fork(fork(x)) will result in a state of complete lock; or None
   * of them not being able to use.
   *
   * Now the second case is fork(map2(fork(x), fork(y)))
   * The outer task is submitted to a thread pool and occupies a thread waiting for
   * fork(x) and fork(y). This is because get function is called in map2. fork(x) and fork(y)
   * will execute in parallel, but there is only one thread available and it results in a deadlock.
   */

  // The rest of the chapter deals with the alternate of fork funtionality, and the obvious change in
  // Future implementation. The main changes are as follows:
  // sealed trait Future[A] {
  // private[parallelism] def apply(k: A => Unit): Unit
  //  }
  // type Par[+A] = ExecutorService => Future[A]

  // We will skip the session as we already covered the core of this chapter.

  //def run(es: ExecutorService)(pr: Par[A]) = ???
  def choice[A](cond: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] = {
    es => if (run(es)(cond).get) f(es) else t(es)
  }

  //Exercise 7.11
  def choiceN[A](int: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es =>
      {
        val n = run(es)(int).get
        if (choices.size >= n)
          choices(n)(es)
        else
          choices(choices.size)(es)
      }
  }

  def choiceInTermsOfChoiceN[A](cond: Par[Boolean])(f: Par[A], t: Par[A]) =
    choiceN[A](map(cond)(if (_) 0 else 1))(List(f, t))

  // Exercise 7.12
  // Please note that it differs from fpinscala. May be u can try to find out the differece. For me this makes more sense as of now
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
    es =>
      {
        val k = run(es)(key).get
        choices(k)(es)
      }
  }

  // Exercise 7.13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es =>
      {
        val k = run(es)(pa).get
        val par = choices(k)
        par(es)
      }
  }

  def choiceUsingChoose[A](cond: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] = {
    chooser[Boolean, A](cond)(if (_) f else t)
  }

  def choiceNUsingChoose[A](n: Par[Int])(list: List[Par[A]]): Par[A] = {
    chooser[Int, A](n)(list(_))
  }

  // join function
  def join[A](a: Par[Par[A]]): Par[A] = es => {
    val parA: Par[A] = a(es).get()
    parA(es)
  }

  //bind or flatMap
  def flatMapUsingJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    val parOfpar: Par[Par[B]] = map(a)(f)
    join(parOfpar)
  }

  // join function
  def joinUsingFlatMap[A](a: Par[Par[A]]): Par[A] = {
    chooser(a)(a => a)
  }

  // Takeaway
  // We described the API in terms of data types and primitive functions. That is merely description.
  // Later the actual concern of running it was dealt as a `run` function.
  // We also learned about combinators. And we implemented different functions in an algebraic passion.
  // This is going to continue in the next few chapters
}