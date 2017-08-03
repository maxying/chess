package chess
package ui

import chess.logic._

object ConsoleDisplay extends Display {
  def piece2uni(p: Piece): Char = p match {
    case King(White, _) => '♔'
    case King(Black, _) => '♚'
    case Queen(White, _) => '♕'
    case Queen(Black, _) => '♛'
    case Bishop(White, _) => '♗'
    case Bishop(Black, _) => '♝'
    case Knight(White, _) => '♘'
    case Knight(Black, _) => '♞'
    case Rook(White, _) => '♖'
    case Rook(Black, _) => '♜'
    case Pawn(White, _) => '♙'
    case Pawn(Black, _) => '♟'
  }

  type Block = List[List[Char]]

  def createSquare(center: Char, radius: Int, color: Color): Block = {
    require(radius > 0)
    val edgeRow = ("-" * (2 * radius + 3)).toList
    val fill = color match { case White => " "; case Black => "█"}
    val fillRow = s"|${fill * (2 * radius + 1)}|".toList
    val centerRow = s"|${fill * radius}$center${fill * radius}|".toList

    val fillBand = List.fill(radius)(fillRow)

    edgeRow +:
      fillBand ++:
      centerRow +:
      fillBand :+
      edgeRow
  }

  def createSuperRow(squares: List[Block]): Block = {
    squares.transpose.map(_.flatten)
  }

  def createBoard(board: Board): String = {
    val coords: Seq[Coord] = for (row <- 1 to 8; col <- 1 to 8) yield (row, col)

    val allSymbols: Seq[Char] = coords.view
      .map(board.occupied.get)          // does the square have a piece?
      .map(opt => opt.map(piece2uni))   // if so, map to the character
      .map(_.getOrElse(' '))            // otherwise, just use a blank space
      .force

    def colorOf(coord: Coord): Color = {
      if ((coord._1 + coord._2) % 2 == 0) Black
      else White
    }

    val coordColors = coords.map(colorOf)

    val allSquares: Seq[Block] = allSymbols.zip(coordColors).map {
      case (chr, color) => createSquare(chr, 1, color)
    }

    val superRows: List[Block] = allSquares.toList
      .grouped(8)           // 8 blocks make a row
      .map(createSuperRow)  // horizontally concat them
      .toList               // iterator to list
      .reverse              // last row of chess board is printed first

    // ~Technically~ not ASCII, but whatevs
    val superRowsAscii = superRows.map {
      _.map(_.mkString)   // each subrow in a superrow gets concatted (List[Char] -> String
       .mkString("\n")    // subrows make the superrow with newlines
    }

    superRowsAscii.mkString("\n")
  }

  override def displayBoard(board: Board): Unit = {
    println(createBoard(board))
  }
}
