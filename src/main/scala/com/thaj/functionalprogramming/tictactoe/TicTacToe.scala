package com.thaj.functionalprogramming.tictactoe

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State
import scalaz._, Scalaz._
/**
 * Created by afsalthaj on 8/7/17.
 */
sealed trait Player
case object X extends Player
case object O extends Player
// A cell is represented by a player

case class Cell(value: Option[Player])

// To be continued, trying Tic Tac Toe
case class Board(cells: List[Cell]) {
  def IsGameOver(board: Board, player: Player): Boolean = {
    // there are 3 scenarios, if player x wins or all the cells are completed
    cells.forall(_.value.isDefined)
  }
}

object Board {
  type BoardState = State[Board, List[Cell]]

  def tishkyumTheBoard(player: Player, index: Int): BoardState = {
    State[Board, List[Cell]](board => {
      val existingList = board.cells
      val newCells = existingList.foldLeft((existingList, 1))((a, b) => {
        if (a._2 == index)
          (Cell(Some(player)) :: a._1, a._2 + 1)
        else
          (b :: a._1, a._2 + 1)
      })
      (newCells._1, Board(newCells._1))
    })
  }
}