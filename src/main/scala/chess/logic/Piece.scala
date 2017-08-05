package chess
package logic

import Moves._

/** For en-passant and castling */
protected trait HasMoved {
  def hasMoved: Boolean
}

sealed trait Piece {
  def color: Color
  def coord: Coord

  def canMoveTo(target: Coord, occupancy: Map[Coord, Piece]): Boolean

  def canAttack(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    canMoveTo(target, occupancy - target)
  }

  def canAttack(that: Piece, occupancy: Map[Coord, Piece]): Boolean = {
    this.color != that.color && canAttack(that.coord, occupancy)
  }
}

final case class King(color: Color, coord: Coord, hasMoved: Boolean)
    extends Piece with HasMoved {

  override def canMoveTo(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    (target notIn occupancy) &&
    (math.abs(target._1 - coord._1) < 1
    || math.abs(target._2 - coord._2) < 1)
  }

}

final case class Queen(color: Color, coord: Coord) extends Piece {

  override def canMoveTo(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    (target notIn occupancy) &&
    (sameRow(coord, target) || sameCol(coord, target) || sameDiag(coord, target)) &&
    clearPath(coord, target, occupancy)
  }

}

final case class Bishop(color: Color, coord: Coord)
    extends Piece {

  override def canMoveTo(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    (target notIn occupancy) &&
    sameDiag(coord, target) &&
    clearPath(coord, target, occupancy)
  }

}

final case class Knight(color: Color, coord: Coord)
    extends Piece {

  override def canMoveTo(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    (target notIn occupancy) && ellShaped(coord, target)
  }

}

final case class Rook(color: Color, coord: Coord, hasMoved: Boolean)
    extends Piece with HasMoved {

  override def canMoveTo(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    (target notIn occupancy) &&
    (sameRow(coord, target) || sameCol(coord, target)) &&
    clearPath(coord, target, occupancy)
  }

}

final case class Pawn(color: Color, coord: Coord, hasMoved: Boolean)
    extends Piece with HasMoved {

  val updown: Int = if (color == White) 1 else -1

  override def canMoveTo(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    (target notIn occupancy) && (
    if (hasMoved) {
      target == (coord._1 + updown, coord._2)
    } else {
      target == (coord._1 + updown, coord._2) || target == (coord._1 + 2 * updown, coord._2)
    })
  }

  override def canAttack(target: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    // TODO: en passant
    target == (coord._1 + updown, coord._2 - 1) ||
    target == (coord._1 + updown, coord._2 + 1)
  }

}
