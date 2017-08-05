package chess.ui

import chess.logic.Board
import chess.ui.ConsoleDisplay._
import org.scalatest.FunSuite

class ConsoleDisplayTest extends FunSuite {
  test("createBoard for initial board") {
    val expected =
      """-----------------
        #|♜|♞|♝|♛|♚|♝|♞|♜|
        #-----------------
        #|♟|♟|♟|♟|♟|♟|♟|♟|
        #-----------------
        #| | | | | | | | |
        #-----------------
        #| | | | | | | | |
        #-----------------
        #| | | | | | | | |
        #-----------------
        #| | | | | | | | |
        #-----------------
        #|♙|♙|♙|♙|♙|♙|♙|♙|
        #-----------------
        #|♖|♘|♗|♕|♔|♗|♘|♖|
        #-----------------""".stripMargin('#')

    assertResult(expected)(createBoard(Board.INIT))

  }


}
