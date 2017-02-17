package com.thaj.functionalprogramming.example.exercises


/**
  * Created by afsalthaj on 8/11/2016.
  */
object PureStatefulAPIGeneric {
  import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI.RNG

  // From the understanding of RNG state actions, maps, flatmaps, sequence etc, we can infer that
  // anything in fucntional programming can be composed, even the stateful results. This is seen to
  // be really seamless to the end user of these functionalities. However, please note that it isn't specific
  // to RNG, but for any action that requires the state to be transmitted across

  // A generic state action map function signature looks like this
  // def map[S, A, B] (action: S => (A, S) (f: A => B):  S => (B, s)

  // Instead of Rand[A], let us define a generic state
  /** {{{
          type State[S, +A] = S => (A, S) === type Rand[+A] = RNG => (A, RNG)
      }}}
    */
  // it is +A instead of A so that even `Nothing` can take part in your state actions
  // Here State is short for computation that carries some state along,
  // or state action, state transition, or even statement
  // (see the next section). We might want to write it as its own class, wrapping the underlying function like this:

  /** Instead of a type State, and state action somewhere in the class,
    * let us have a state class that wraps the state action
    */

  // Exercise 6.10
  case class State[S, +A] (run: S =>  (A, S)) {
    def map[B] (f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))



  }

  //or type Rand[A] = State[RNG, A]

  object State {
    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    def sequence[S, A] (a: List[State[S, A]]): State[S, List[A]] =
      a.foldLeft(unit[S, List[A]](Nil))((a,b) => b.map2(a)(_ :: _))

    // Reimplementing RNG using Generic State Action
    type Rand[A] = State[RNG, A]
    def both[A, B] (a: Rand[A], b: Rand[B]): Rand[(A, B)] =
    // map2(a, b)((a,b) => (a,b))
    a.map2(b)((_,_))


    val int: Rand[Int] = State(t => t.nextInt)
    def nonNegativeInt: Rand[Int] = int.map(t => if (t < 0) -t-1 else t)
    def double:Rand[Double] = nonNegativeInt.map(a => a/Int.MaxValue.toDouble+1)

    //reimplementing double
    def doubleInt: Rand[(Double, Int)] = both(double, int)
    def intDouble: Rand[(Int, Double)] = both(int, double)

    def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

    // Please cover PurelyFunctionaImperativeProgramming befor you read the below functions
    // The get action simply passes the incoming state along and returns it as the value:
    def get[S]: State[S, S] = State(s => (s, s))
    //The set action is constructed with a new state s. The resulting action ignores the incoming state,
    // replaces it with the new state, and returns () instead of a meaningful value:
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  }
}

// Another straight forward implementation, that covers end to end
// Difference is only in map implementation
/***
   * {{{
     case class State[S, +A] (run: S => (A, S)) {
       def map[B](f: A => B): State[S, B] = State(state => {
       val (a, r) = run(state)
       (f(a), r)
     })

     def flatMap[B] (f: A => State[S, B]) = State(state => {
       val (a, s) = run(state)
       f(a).run(s)
     })

     def map2[B, C](state: State[S, B])(f: (A, B) => C): State[S, C] =
       flatMap(aa => state.map(cc => f(aa, cc)))
     }

   *
   * }}}
   */
