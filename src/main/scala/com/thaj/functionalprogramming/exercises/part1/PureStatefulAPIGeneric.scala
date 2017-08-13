package com.thaj.functionalprogramming.example.exercises

/**
  * Created by afsalthaj on 8/11/2016.
  */
object PureStatefulAPIGeneric {

  import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI.RNG

  // From the understanding of RNG state actions, maps, flatmaps, sequence etc, I can infer that
  // anything in fucntional programming can be composed, even the stateful results. This is seen to
  // be really seamless to the end user of these functionalities. However, please note that it isn't specific
  // to RNG, but for any action that requires the state to be transmitted across

  // A generic state action map function signature looks like this
  // def map[S, A, B] (action: S => (A, S) (f: A => B):  S => (B, s)

  // Instead of Rand[A], let us define a generic state
  /** {{{
    * type State[S, +A] = S => (A, S) === type Rand[+A] = RNG => (A, RNG)
    * }}}
    */
  // it is +A instead of A so that even `Nothing` can take part in your state actions
  // Here State is short for computation that carries some state along,
  // or state action, state transition, or even statement
  // (see the next section). I might want to write it as its own class, wrapping the underlying function like this:

  /** Instead of a type State, and state action somewhere in the class,
    * let us have a state class that wraps the state action
    */

  // Exercise 6.10
  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

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

    def sequence[S, A](a: List[State[S, A]]): State[S, List[A]] =
      a.foldLeft(unit[S, List[A]](Nil))((a, b) => b.map2(a)(_ :: _))

    // Reimplementing RNG using Generic State Action
    type Rand[A] = State[RNG, A]

    def both[A, B](a: Rand[A], b: Rand[B]): Rand[(A, B)] =
      a.map2(b)((_, _))


    def boolean: Rand[Boolean] = State(rng =>
      rng.nextInt match {
        case (i, rng2) => (i % 2 == 0, rng2)
      })


    val int: Rand[Int] = State(t => t.nextInt)

    def nonNegativeLessThanIntState(n: Int): Rand[Int] = int.map { t =>
      val mod = t % n
      if (t + (n - 1) - mod >= 0)
        mod
      else t
    }

    def nonNegativeInt: Rand[Int] = int.map(t => if (t < 0) -t - 1 else t)

    def double: Rand[Double] = nonNegativeInt.map(a => a / Int.MaxValue.toDouble + 1)

    //reimplementing double
    def doubleInt: Rand[(Double, Int)] = both(double, int)

    def intDouble: Rand[(Int, Double)] = both(int, double)

    def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

    // Please cover PurelyFunctionaImperativeProgramming before you read the below functions
    // The get action simply passes the incoming state along and returns it as the value:
    def get[S]: State[S, S] = State(s => (s, s))

    //The set action is constructed with a new state s. The resulting action ignores the incoming state,
    // replaces it with the new state, and returns () instead of a meaningful value:
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  }

}

// Another straight forward implementation, that covers end to end
// Difference is only in map implementation
/** *
  * {{{
  * case class State[S, +A] (run: S => (A, S)) {
  * def map[B](f: A => B): State[S, B] = State(state => {
  * val (a, r) = run(state)
  * (f(a), r)
  * })
  * *
  * def flatMap[B] (f: A => State[S, B]) = State(state => {
  * val (a, s) = run(state)
  * f(a).run(s)
  * })
  * *
  * def map2[B, C](state: State[S, B])(f: (A, B) => C): State[S, C] =
  * flatMap(aa => state.map(cc => f(aa, cc)))
  * }
  *
  *
  * }}}
  */

/**
  * Should you know why we have something known as `scalaz.ST` then please read the below contents, else skip!
  * "http://docplayer.net/404814-Lazy-functional-state-threads.html#show_full_text" should be a good read to
  * understand how mutations are handled nicely in FP paradigm. Also, if you are looking for a quick
  * explanation on why State monad is NOT used instead of ST monad in scala for handling mutations,
  * then below code explanations might help.
  */
object MutationWithState {

  // A mutation handling using state monad. Assume that the mutation that you need to perform is encapsulated
  // in below container. The container is obviously represented by State S and the actual value A.
  case class MutationContainer[S, A](a: A) {
    private var value: A = a

    // read the value associating it with only the state and not the MutationContainer.
    def readValue: scalaz.State[S, A] = scalaz.State.state(a)

    // mutate the container, returning a container.
    def mutateThis(a: A): scalaz.State[S, MutationContainer[S, A]] = scalaz.State(s => {
      value = a
      (s, this)
    })

    // modify the container using `mutateThis` and return a container
    def modify(f: A => A): scalaz.State[S, MutationContainer[S, A]] = for {
      value <- readValue
      //f(value) can mutating a value
      newContainer <- mutateThis(f(value))
    } yield newContainer
  }

  private def newMutationContainer[S, A](a: => A) = scalaz.State.state[S, MutationContainer[S, A]](MutationContainer[S, A](a))

  // mutating my variable, the access to mutation container is prohibited, nor getting its value
  // in between the chain of operations is prohibited. As far as it is prohibited, I am guessing it is
  // referentially transparent
  val x = 10

  // Oh that's ugly. Now this is where we'l be in trouble trying to re-use the state DataStructure. 
  // Would the initial state really work? Probably Not. I think a dummy value
  // representing a state should be a dummy instantiation of a class that represents the state with no data in it (explained later)
  val dummyVariable: Nothing = "".asInstanceOf[Nothing]

  // natural transformation kind of thing...Quantifying a type universally! 
  trait Forall[P[_]] {
    def apply[A]: P[A]
  }

  // this is similar to scalaz's implementation of Run ST where it specifically takes care of
  def myRunST[A](f: Forall[({type λ[S] = scalaz.State[S, A]})#λ]): A =
    f.apply.run(dummyVariable)._2

  /** we are forced to run with a dummyValue that actually represents the state
    * However our state is just a type parameter S. 
    * So had the state been a case class with no data in it, we could easily
    * make a dummy instance of it with no data. This is exactly what scalaz does in
    * `World.scala`. 
    *
    * {{{
    *   case class Tower[S]()...it should be a state thread that should handle a mutation, and noone else
    *   can come into that thread, and change the value that is already being mutated. This is why it is type-parameterized by S.
    * }}}
    */

  def e1[S]: scalaz.State[S, MutationContainer[S, Int]] = for {
    r <- newMutationContainer[S, Int](0)
    x <- r.modify(_ + 1)
  } yield x

  // e1.run(())._2 ?? well quite nasty!

  def e2[A]: scalaz.State[A, Int] = e1[A].flatMap(_.readValue)

  // the below one won't compile as it exposes the mutation container
  //myST(new Forall[({type λ[S] = scalaz.State[S, MutationContainer[S, Int]]})#λ] {
  // def apply[A] = e1
  //})

  // the below one compiles as expected, however the representation of State in this case is quite nasty.
  // In the case of a mutation, the usage of state is to make them act like a state thread - this
  // somehow proved that, we can't directly use the data structure that we used to represent the state Monad
  // wouldn't work here.
  myRunST(new Forall[({type λ[S] = scalaz.State[S, Int]})#λ] {
    def apply[A] = e2
  })
}

/**
  * the below session is quite advanced. however, if you are wondering why scalaz.State (or the above the State
  * data structure) can't be directly used instead of ST monad for working with in place swapping, mutation etc.
  * This is just a simplified version of scalaz's ST. This is in line with the explanation of Scala red book.
  */
object MutationInScalaz {

  // So for the above reasons we need to represent the state
  // in a way that we should be able to create a dummy data with no content.
  // This can't be a case class Something(), but it should be a case class Something[S]()
  // that S can vary from time to time after every mutation. And the dummy value can be
  // SomethingRepresentingState[SomeInstance](). In scalaz, `SomethingRepresentingState` is `Tower` and `SomeInstance` is
  // `IvoryTower`. In this example SomethingRepresentingState is World and a dummy state is `Antartica`  - makes more
  // sensible for me atleast.
  case class World[A]()

  case class STX[S, A](f: World[S] => (World[S], A)) {
    def apply(s: World[S]) = f(s)

    def flatMap[B](g: A => STX[S, B]): STX[S, B] =
      STX(s => f(s) match {
        case (ns, a) => g(a)(ns)
      })

    def map[B](g: A => B): STX[S, B] =
      STX(s => f(s) match {
        case (ns, a) => (ns, g(a))
      })
  }

  def returnST[S, A](a: => A): STX[S, A] = STX(s => (s, a))

  case class STRef[S, A](a: A) {
    private var value: A = a

    def read: STX[S, A] = returnST(value)

    def write(a: A): STX[S, STRef[S, A]] = STX((s: World[S]) => {
      value = a;
      (s, this)
    })

    def mod[B](f: A => A): STX[S, STRef[S, A]] = for {
      a <- read
      v <- write(f(a))
    } yield v
  }

  def newVar[S, A](a: => A): STX[S, STRef[S, A]] = returnST(STRef[S, A](a))

  trait Forall[P[_]] {
    def apply[A]: P[A]
  }

  sealed trait Antartica

  val antartica = World[Antartica]()

  def runSTT[A](f: Forall[({type λ[S] = STX[S, A]})#λ]): A =
    f.apply.f(antartica)._2

  def e1[S]: STX[S, STRef[S, Int]] = for {
    r <- newVar[S, Int](0)
    x <- r.mod(_ + 1)
  } yield x

  def eArray[S]: STX[S, STRef[S, Array[Int]]] =  for {
    r <- newVar[S, Array[Int]](Array(1, 2, 3))
    x <- r.mod(ty => {ty(2) = 1; ty})
  } yield x

  def eArrayRead[A]: STX[A, Array[Int]] = eArray[A].flatMap(_.read)
  // this is nicer and compiles
  def e2[A]: STX[A, Int] = e1[A].flatMap(_.read)

  // but this doesn't compile, since it exposes the STRef
  // The whole complexity comes in through
  // runSTT(new Forall[({type λ[S] = STX[S, STRef[S, Int]]})#λ] { def apply[A] = e1 })
  // this compiles as it doesn't expose the ST
  val sample = runSTT(new Forall[({type λ[S] = STX[S, Int]})#λ] {
    def apply[A] = e2
  })

  val sample1 = runSTT(new Forall[({type λ[S] = STX[S, Array[Int]]})#λ] {
    def apply[A] = eArrayRead
  })
}
