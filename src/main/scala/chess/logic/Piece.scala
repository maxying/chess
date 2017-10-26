package chess.logic

sealed trait Piece { def color: Color }
  final case class Pawn(color: Color, moved: Boolean) extends Piece
  final case class Rook(color: Color, moved: Boolean) extends Piece
  final case class Knight(color: Color) extends Piece
  final case class Bishop(color: Color) extends Piece
  final case class Queen(color: Color) extends Piece
  final case class King(color: Color, moved: Boolean) extends Piece
