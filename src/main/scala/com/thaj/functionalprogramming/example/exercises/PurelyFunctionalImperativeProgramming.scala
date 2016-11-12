package com.thaj.functionalprogramming.example.exercises

/**
  * Created by afsalthaj on 12/11/2016.
  */
/**
  * In general, state action takes a state, generates a value using the state along with
  * its new state. We wrapped in State class, to have programmatic resemblance to any types
  * in Functional Programming with its inherent map, map2, sequence functions that composed
  * all these state actions. In the back end, what exactly happens is an imperative style
  * actions.
  * For example
  * Let us define the State again:
  *  {{{ case class State[S, +A] (run: S => (S, A)) }}}
  * Now you can compose two state actions using map2
  *   {{{ def map2[S, A, B](state1:State[S, A], state2: State[S, B]) (f:(A, B) => C): State[S,C] =
  *         State (givenStateInstance => {
  *          val (newValue, newStateInstance) = state1.run(state)
  *          val (anotherValue, newestState) = state2.run(newStateInstance)
  *          (f(newValue, anotherValue), newestState)
  *        })
  *
  *        def map[S, B](state: State[S, A], f: A => B) = State (state => {
  *          val (value, newState) = state.run(state)
  *          (f(value), newState)
  *        }
  *   }}}
  *
  * Now you RNG make use of the above state representation to define its composed state actions
  * For example:
  * {{{
  *     type Rand[A] = State[RNG, A]
  *     val int: Rand[Int] = _.nextInt
  *     val double: Rand[Double] = map(int, _.toDouble)
  *     def both(a: Rand[A], Rand[B])= map2(a, b)((value1, value2) => (value1, value2))
  *     def doubleInt: Rand[(Double, Int)] = both(double, int)
  * }}}
  *
  * You consider the above actions as a revision to Stateful API chapter. And you can infer that,
  * We’d run a state action, assign its result to a val, then run another state action that used that val,
  * assign its result to another val, and so on behind the scene.
  *
  * In the imperative programming paradigm, a program is a sequence of statements where each statement
  * may modify the program state. That’s exactly what we’ve been doing, except that our “statements”
  * are really State actions, which are really functions. As functions, they read the current program state
  * simply by receiving it in their argument,
  * and they write to the program state simply by returning a value
  *
  * Aren’t imperative and functional programming opposites?
  * Absolutely not. Remember, functional programming is simply programming without side effects.
  * Imperative programming is about programming with statements that modify some program state,
  * and as we’ve seen, it’s entirely reasonable to maintain state without side effects.
  * Functional programming has excellent support for writing imperative programs,
  * with the added benefit that such programs can be reasoned about equationally because
  * they’re referentially transparent
  */
object PurelyFunctionalImperativeProgramming {

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State._

  // Please try to understand what is happening here; a complex representation
  val ns: Rand[List[Int]]= int.flatMap( someCount => int.flatMap(value => ints(someCount).map(xs => xs.map(_ % value))))

  // Please try understand the same operation using for comprehension

  /**
    * This code is much easier to read (and write),
    * and it looks like what it is—an imperative program that maintains some state.
    * But it’s the same code. We get the next Int and assign it to x, get the next Int after that
    * and assign it to y, then generate a list of length x, and finally return the list with all of its
    * elements modulo y elements modulo y.
    */
  val nsSimple: Rand[List[Int]] = for {
    x <- int
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % y)
}
