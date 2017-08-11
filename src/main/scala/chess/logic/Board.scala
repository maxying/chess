package chess
package logic

case class Board(pieces: Set[Piece]) {
  val self: Map[Coord, Piece] = pieces.map(p => (p.coord, p)).toMap

  def findKing(color: Color): Piece = {
    pieces
      .find(p => p.isInstanceOf[King] && p.color == color)
      .getOrElse(throw new IllegalStateException(s"$color King is missing from the Board!"))
  }

  def piecesAttacking(color: Color, coord: Coord): Set[Piece] = {
    pieces.filter(p => p.color == color && p.canAttack(coord, this))
  }

  def occupied(coord: Coord): Boolean = self.contains(coord)

  def unoccupied(coord: Coord): Boolean = !occupied(coord)

  def inCheck(color: Color): Boolean = {
    val king = findKing(color)
    piecesAttacking(color.opp, king.coord).nonEmpty
  }

  def allActions: Set[(Piece, Action)] = {
    for {
      piece <- pieces
      action <- piece.allActions(this)
    } yield (piece, action)
  }

  def applyAction(piece: Piece, action: Action): Board = action match {
    case BasicMove(target) => Board(pieces - piece + piece.applyAction(action))
    case Capture(victim) => Board(pieces - victim + piece.applyAction(action))
  }

}

object Board {
  val INIT: Board = {
    def pawns: Seq[Piece] = (1 to 8).flatMap { col =>
      List(
        Pawn(White, (2, col), hasMoved = false),
        Pawn(Black, (7, col), hasMoved = false))
    }
    def royalty: Seq[Piece] = List(
      Queen(White, (1, 4)),
      King(White, (1, 5), hasMoved = false),
      Queen(Black, (8, 4)),
      King(Black, (8, 5), hasMoved = false)
    )
    def others: Seq[Piece] = List(
      Rook(White, (1, 1), hasMoved = false), Rook(White, (1, 8), hasMoved = false),
      Knight(White, (1, 2)), Knight(White, (1, 7)),
      Bishop(White, (1, 3)), Bishop(White, (1, 6)),
      Rook(Black, (8, 1), hasMoved = false), Rook(Black, (8, 8), hasMoved = false),
      Knight(Black, (8, 2)), Knight(Black, (8, 7)),
      Bishop(Black, (8, 3)), Bishop(Black, (8, 6))
    )
    createFrom(pawns ++ royalty ++ others)
  }

  def createFrom(pieces: Iterable[Piece]): Board = {
    Board(pieces.toSet)
  }

}
