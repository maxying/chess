package chess
package ui

import chess.logic._

object ConsoleDisplay extends Display {
  def piece_symbol(p: Piece): Char = p match {
    case King(White, _) => '♔'
    case King(Black, _) => '♚'
    case Queen(White) => '♕'
    case Queen(Black) => '♛'
    case Bishop(White) => '♗'
    case Bishop(Black) => '♝'
    case Knight(White) => '♘'
    case Knight(Black) => '♞'
    case Rook(White, _) => '♖'
    case Rook(Black, _) => '♜'
    case Pawn(White, _) => '♙'
    case Pawn(Black, _) => '♟'
  }

  def createBoard(board: Board): String = {
    val rows = for (r_idx <- 8 to 1 by -1) yield {
      (1 to 8)
        .map(c_idx =>
          board.board
            .get((r_idx, c_idx))
            .fold(' ')(piece_symbol)).
        mkString(s"$r_idx |", "|", "|\n")
    }

    val row_sep = "-" * (rows(0).length - 1) + "\n"

    val col_label = (1 to 8).mkString(start="  |", sep="|", end="|")

    rows.mkString(row_sep, row_sep, row_sep) + col_label
  }

  override def displayBoard(board: Board): Unit = {
    println(createBoard(board))
  }
}
