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
 * Now let RNG make use of the above state representation to define its composed state actions
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

  import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State, State._

  // Please try to understand what is happening here; a complex representation
  val ns: Rand[List[Int]] = int.flatMap(someCount => int.flatMap(value => ints(someCount).map(xs => xs.map(_ % value))))

  // Please try understand the same operation using for comprehension

  /**
   * This code is much easier to read (and write),
   * and it looks like what it is—an imperative program that maintains some state.
   * But it’s the same code. We get the next Int and assign it to x, get the next Int after that
   * and assign it to y, then generate a list of length x, and finally return the list with all of its
   * elements modulo y
   */
  val nsSimple: Rand[List[Int]] = for {
    someCount <- int
    value <- int
    xs <- ints(someCount)
  } yield xs.map(_ % value)

  /**
   *  If we imagine that we have a combinator get for getting the current state, and a combinator
   *  set for setting a new state, we could implement a combinator that can modify the state in arbitrary ways:
   */

  def modifyS[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  //Exercise 6.11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def operateOnMachine(input: Input) = input match {
      case _ if candies == 0 => this
      case Coin if locked    => Machine(false, candies, coins + 1)
      case Turn if !locked   => Machine(true, candies - 1, coins)
      case _                 => this
    }
  }

  /**
   * The rules of the machine are as follows:
   * Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
   * Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
   * Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
   * A machine that’s out of candy ignores all inputs.
   */

  type MachineState = State[Machine, (Int, Int)]

  /**
   * The method simulateMachine should operate the machine based on the list of inputs
   * and return the number of coins and candies left in the machine at the end. For example,
   * if the input Machine has 10 coins and 5 candies,
   * and a total of 4 candies are successfully bought, the output should be (14, 1).
   *
   * {{{
   *       def get[S]: State[S, S] = State(s => (s, s))
   * //The set action is constructed with a new state s. The resulting action ignores the incoming state,
   * // replaces it with the new state, and returns () instead of a meaningful value:
   * def set[S](s: S): State[S, Unit] = State(_ => ((), s))
   * }}}
   */

  // The author's implementation in fpinscala
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(input => modifyS[Machine](_.operateOnMachine(input))))
    machine <- get
  } yield (machine.coins, machine.candies)

  // OR for better understanding
  def simulateMachineAlternate(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val s: State[Machine, List[Unit]] =
      sequence(inputs.map(input => State[Machine, Unit](a => ((), a.operateOnMachine(input)))))

    // setting the state
    val newState: State[Machine, Machine] = s.flatMap(_ => State[Machine, Machine](t => (t, t)))

    // get the state
    newState.map(s => (s.coins, s.candies))
  }

  //Example usage
  val lockedMachine = Machine(locked = true, 10, 20)
  val unlockedMachine = Machine(locked = false, 10, 20)
  val emptyLockedMachine = Machine(locked = true, 0, 20)
  val emptyUnlockedMachine = Machine(locked = false, 0, 20)

  val assertions = {
    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    assert(!simulateMachine(List(Coin)).run(lockedMachine)._2.locked)

    // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    assert(simulateMachine(List(Turn)).run(unlockedMachine)._2.locked)
    assert(simulateMachine(List(Turn)).run(unlockedMachine)._2.candies == 9)

    // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    assert(simulateMachine(List(Turn)).run(lockedMachine)._2 == lockedMachine)
    assert(simulateMachine(List(Coin)).run(unlockedMachine)._2 == unlockedMachine)

    // A machine that’s out of candy ignores all inputs.
    assert(simulateMachine(List(Turn)).run(emptyLockedMachine)._2 == emptyLockedMachine)
    assert(simulateMachine(List(Turn)).run(emptyUnlockedMachine)._2 == emptyUnlockedMachine)
    assert(simulateMachine(List(Coin)).run(emptyLockedMachine)._2 == emptyLockedMachine)
    assert(simulateMachine(List(Coin)).run(emptyUnlockedMachine)._2 == emptyUnlockedMachine)

  }

  /**
   * A simple use case: Refer Monad before u jump here
   * @param carbon
   * @param db
   */
  // a fatty acid basically represents the number of carbons and double bonds
  case class FattyAcid(carbon: Int, db: Int)

  val initialFattyAcid = FattyAcid(18, 1)
  // An enzymatic reaction is something which is a state action that converts an fatty acid to another fatty acid
  type EnzymaticReaction = State[FattyAcid, List[FattyAcid]]

  type Reaction = FattyAcid => FattyAcid

  val reaction1 = (fa: FattyAcid) => fa match {
    case (FattyAcid(cs, dbs)) => FattyAcid(cs + 2, dbs)
  }
  val reaction2 = (fa: FattyAcid) => fa match {
    case (FattyAcid(cs, dbs)) => FattyAcid(cs - 2, dbs)
  }

  object EnzymaticReaction {
    def unit(x: List[FattyAcid]): EnzymaticReaction = State[FattyAcid, List[FattyAcid]](f => (x, f))
    // def chainOfEnzymaticReaction(fattyAcid: FattyAcid): State[FattyAcid, FattyAcid] =
    // def convertAListOfReactions(reactions: List[Reaction]) =

    def getStreamOfFattyAcidStates(listOfReactions: List[Reaction], start: FattyAcid, end: FattyAcid): (List[FattyAcid], FattyAcid) =
      listOfReactions.foldLeft(unit(List[FattyAcid]()): EnzymaticReaction)((accState, reaction) => {
        for {
          facids <- accState
          facid <- get
          _ <- set(reaction(facid))
        } yield facid :: facids
      }).run(start)
  }
}

// Take away from Chapter 6:
// You can define stateful APIs in FP
// Always return the new state along with the desired value, instead of mutating a given state
// A state action is always S => (S, A)
// Wrap it in a box called State which is State(run: S => (S,A))
// Define sequence, map, flatMap etc for this box
// Represent RNG state actions in terms of State
// And then you are discovering the fact that all your RNG state actions can be chained, composed, or combined
// easily through the functions of the box `State`