package chess
package ui

import chess.logic._

object ConsoleDisplay extends Display {
  def piece2uni(p: Piece): Char = p match {
    case King(White, _, _) => '♔'
    case King(Black, _, _) => '♚'
    case Queen(White, _) => '♕'
    case Queen(Black, _) => '♛'
    case Bishop(White, _) => '♗'
    case Bishop(Black, _) => '♝'
    case Knight(White, _) => '♘'
    case Knight(Black, _) => '♞'
    case Rook(White, _, _) => '♖'
    case Rook(Black, _, _) => '♜'
    case Pawn(White, _, _) => '♙'
    case Pawn(Black, _, _) => '♟'
  }

  type Square = List[List[Char]]

  def createSquare(center: Char): Square = List(
    List('-', '-'),
    List('|', center)
  )

  def createSuperRow(squares: List[Square]): Square = {
    squares.transpose.map(_.flatten)
  }

  def createBoard(board: Board): String = {
    val coords: Seq[Coord] = for (row <- 1 to 8; col <- 1 to 8) yield (row, col)

    val allSymbols: Seq[Char] = coords.view
      .map(board.self.get)              // does the square have a piece?
      .map(opt => opt.map(piece2uni))   // if so, map to the character
      .map(_.getOrElse(' '))            // otherwise, just use a blank space
      .force

    val allSquares: Seq[Square] = allSymbols.map(createSquare)

    val superRows: List[List[List[Char]]] = allSquares.toList
      .grouped(8)           // 8 blocks make a row
      .map(createSuperRow)  // horizontally concat them
      .toList               // iterator to list
      .reverse              // last row of chess board is printed first

    // The center of every 3 fine-rows is the row of "squares".
    // Above and below is just line padding
    val fineRows: List[List[Char]] = superRows.flatten

    val rowEndings: Stream[Char] = Stream.continually(List('-', '|')).flatten

    val withEndings: List[List[Char]] = fineRows
      .zip(rowEndings)
      .map { case (finerow, ending) => finerow :+ ending }

    // ~Technically~ not ASCII, but whatevs
    val asciiBoard: String = withEndings.map(_.mkString).mkString("\n")

    val footer = "\n" + "-" * 17

    asciiBoard + footer
  }

  override def displayBoard(board: Board): Unit = {
    println(createBoard(board))
  }
}
