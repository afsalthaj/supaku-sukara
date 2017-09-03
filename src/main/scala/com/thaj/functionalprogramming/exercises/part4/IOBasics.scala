package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad

/**
  * We’ll make an important distinction in this chapter between effects and side effects.
  * The IO monad provides a straightforward way of embedding imperative pro- gramming with I/O effects
  * in a pure program while preserving referential transpar- ency. It clearly separates effectful code—code
  * that needs to have some effect on the outside world—from the rest of our program.
  * This will also illustrate a key technique for dealing with external effects—using pure functions to compute a
  * description of an effectful computation, which is then executed by a separate interpreter that actually
  * performs those effects. Essentially we’re crafting an embedded domain-specific language (EDSL) for
  * imperative pro- gramming. This is a powerful technique that we’ll use throughout the rest of part 4.
  * Our goal is to equip you with the skills needed to craft your own EDSLs for describ- ing effectful programs.
  */

/**
  * The insight here is that inside every function with side effects is a pure function waiting to get out.
  * We can formalize this insight a bit. Given an impure function f of type A => B, we
  * can split f into two functions:
  * A pure function of type A => D, where D is some description of the result of f.
  * An impure function of type D => B, which can be thought of as an interpreter of these descriptions.
  */

case class Player(name: String, score: Int)

object IOBasics {
  // A simple IO
  trait IO {
    def run: Unit
  }

  object IO {
    def winner(player1: Player, player2: Player) =
      if (player1.score > player2.score)
        Some(player1)
      else if (player2.score > player1.score)
        Some(player2)
      else None

    def winnerMsg(player: Option[Player]) =
      player.map(t => s"player ${t.name} is the winner").getOrElse("It is a draw")

    def printLine(msg: String): IO = new IO {
      def run = println(msg)
    }

    /**
      * We say that contest has (or produces) an effect or is effectful, but it’s only the
      * interpreter of IO (its run method) that actually has a side effect.
      * The responsibility of interpreting the effect and actually manipulating the console is
      * held by the run method on IO.
      */
    def contest(p1: Player, p2: Player): IO = {
      printLine(winnerMsg(winner(p1, p2)))
    }
    //... on to 13.2.1
  }
}