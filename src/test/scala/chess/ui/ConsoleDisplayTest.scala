package chess.ui

import chess.logic.{Black, White}
import chess.ui.ConsoleDisplay._
import org.scalatest.FunSuite

class ConsoleDisplayTest extends FunSuite {
  test("createSquare negative radius") {
    assertThrows[IllegalArgumentException] {
      createSquare(' ', -1, White)
    }
  }

  test("createSquare 0 radius") {
    assertThrows[IllegalArgumentException] {
      createSquare(' ', 0, White)
    }
  }

  test("createSquare radius 1 white") {
    val expected: List[List[Char]] =
      """-----
        ||   |
        || @ |
        ||   |
        |-----""".stripMargin.split("\n").toList.map(_.toList)

    assertResult(expected)(createSquare('@', 1, White))
  }

  test("createSquare radius 2 black") {
    val expected: List[List[Char]] =
      """-------
        ||█████|
        ||█████|
        ||██@██|
        ||█████|
        ||█████|
        |-------""".stripMargin.split("\n").toList.map(_.toList)

    assertResult(expected)(createSquare('@', 2, Black))
  }

  test("createSquare radius 3 white") {
    val expected: List[List[Char]] =
      """---------
        ||       |
        ||       |
        ||       |
        ||   +   |
        ||       |
        ||       |
        ||       |
        |---------""".stripMargin.split("\n").toList.map(_.toList)

    assertResult(expected)(createSquare('+', 3, White))
  }

  test("createSuperRow 2 squares") {
    val squares = List(
      List(List('1','2'),
           List('3','4')), List(List('5','6'),
                                List('7','8')))
    val expected = List(
      List('1', '2', '5', '6'),
      List('3', '4', '7', '8'))
    assertResult(expected)(createSuperRow(squares))
  }

  test("createSuperRow 3 squares") {
    val squares = List(
      List(List('1','2'),
           List('3','4')), List(List('5','6'),
                                List('7','8')), List(List('9', 'A'),
                                                     List('B', 'C')))
    val expected = List(
      List('1', '2', '5', '6', '9', 'A'),
      List('3', '4', '7', '8', 'B', 'C'))
    assertResult(expected)(createSuperRow(squares))
  }

}
