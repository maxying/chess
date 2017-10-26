package chess.ui

import org.scalatest.FunSuite

import chess.logic.Board
import chess.ui.ConsoleDisplay._

class ConsoleDisplayTest extends FunSuite {
  test("createBoard for initial board") {
    val expected =
      """-------------------
        #8 |♜|♞|♝|♛|♚|♝|♞|♜|
        #-------------------
        #7 |♟|♟|♟|♟|♟|♟|♟|♟|
        #-------------------
        #6 | | | | | | | | |
        #-------------------
        #5 | | | | | | | | |
        #-------------------
        #4 | | | | | | | | |
        #-------------------
        #3 | | | | | | | | |
        #-------------------
        #2 |♙|♙|♙|♙|♙|♙|♙|♙|
        #-------------------
        #1 |♖|♘|♗|♕|♔|♗|♘|♖|
        #-------------------
        #  |1|2|3|4|5|6|7|8|""".stripMargin('#')

    assertResult(expected)(createBoard(Board.INITIAL))

  }


}
