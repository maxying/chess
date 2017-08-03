package chess
package logic

sealed trait Color
case object White extends Color
case object Black extends Color

/** For en-passant and castling */
trait HasMoved {
  var hasMoved = false
}

sealed trait Piece {
  def color: Color
  def coord: Coord
  def canAttack(target: Coord, occupancy: Map[Coord, Piece]): Boolean
  def canAttack(that: Piece, occupancy: Map[Coord, Piece]): Boolean = {
    this.color != that.color && canAttack(that.coord, occupancy)
  }
}

final case class King(color: Color, coord: Coord)
    extends Piece with HasMoved {
  override def canAttack(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    ???
  }
}

final case class Queen(color: Color, coord: Coord) extends Piece {
  override def canAttack(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    ???
  }
}

final case class Bishop(color: Color, coord: Coord) extends Piece {
  override def canAttack(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    ???
  }
}

final case class Knight(color: Color, coord: Coord) extends Piece {
  override def canAttack(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    ???
  }
}

final case class Rook(color: Color, coord: Coord) extends Piece with HasMoved {
  override def canAttack(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    ???
  }
}

final case class Pawn(color: Color, coord: Coord) extends Piece with HasMoved {
  override def canAttack(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    ???
  }
}
