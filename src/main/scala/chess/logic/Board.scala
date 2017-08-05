package chess
package logic

case class Board(self: Map[Coord, Piece]) {}

object Board {
  def createFrom(pieces: Iterable[Piece]): Board = {
    Board(pieces
      .groupBy(_.coord)
      .map { case (coord, wrapped) => (coord, wrapped.head) })
  }

  def INIT: Board = {
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

}
