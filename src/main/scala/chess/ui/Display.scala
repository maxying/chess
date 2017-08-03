package chess.ui

import chess.logic.Board

trait Display {
  def displayBoard(board: Board): Unit
}
