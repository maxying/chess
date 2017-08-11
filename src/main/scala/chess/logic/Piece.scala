package chess
package logic

import Action._

/** For en-passant and castling */
protected trait HasMoved {
  def hasMoved: Boolean
}

sealed trait Piece {
  def color: Color
  def coord: Coord

  protected def movement: Set[Coord]

  def applyAction(action: Action): Piece

  def allActions(board: Board): Set[Action] = {
    def mapToAction(coord: Coord): Option[Action] = {
      val kingSafe = this match {
        case _: King => board.piecesAttacking(color.opp, coord).isEmpty
        case _ => true
      }
      if (kingSafe && board.unoccupied(coord))
        Some(BasicMove(coord))
      else if (kingSafe && board.occupied(coord) && this.canCapture(board.self(coord), board))
        Some(Capture(board.self(coord)))
      else
        None
    }

    movement.flatMap(mapToAction)
  }

  def canMoveTo(target: Coord, board: Board): Boolean = {
    val knightsCanJump = this match {
      case _: Knight => true
      case _ => clearPath(coord, target, board)
    }
    knightsCanJump && movement.contains(target)
  }

  def canAttack(target: Coord, board: Board): Boolean = {
    canMoveTo(target, board)
  }

  def canCapture(that: Piece, board: Board): Boolean = {
    this.color != that.color && canAttack(that.coord, board)
  }
}

final case class King(color: Color, coord: Coord, hasMoved: Boolean)
    extends Piece with HasMoved {
  // TODO: Castling
  override def movement: Set[Coord] = {
    (for {
      dx <- -1 to 1
      dy <- -1 to 1
      if !(dx == 0 && dy == 0)
    } yield (coord._1 + dx, coord._2 + dy)).toSet
  }

  override def applyAction(action: Action): Piece = action match {
    case BasicMove(target) => this.copy(coord=target, hasMoved=true)
    case Capture(victim) => this.copy(coord=victim.coord, hasMoved=true)
  }

}

final case class Queen(color: Color, coord: Coord)
    extends Piece {
  override def movement: Set[(Int, Int)] = {
    coordsInRow(coord) ++ coordsInCol(coord) ++ coordsInDiags(coord)
  }

  override def applyAction(action: Action): Piece = {
    case BasicMove(target) => this.copy(coord=target)
    case Capture(victim) => this.copy(coord=victim.coord)
  }
}

final case class Bishop(color: Color, coord: Coord)
    extends Piece {
  override def movement: Set[Coord] = coordsInDiags(coord)

  override def applyAction(action: Action): Piece = {
    case BasicMove(target) => this.copy(coord=target)
    case Capture(victim) => this.copy(coord=victim.coord)
  }
}

final case class Knight(color: Color, coord: Coord)
    extends Piece {
  override def movement: Set[Coord] = coordsInEll(coord)

  override def applyAction(action: Action): Piece = {
    case BasicMove(target) => this.copy(coord=target)
    case Capture(victim) => this.copy(coord=victim.coord)
  }
}

final case class Rook(color: Color, coord: Coord, hasMoved: Boolean)
    extends Piece with HasMoved {
  // TODO: Castling
  override def movement: Set[Coord] = coordsInRow(coord) ++ coordsInCol(coord)

  override def applyAction(action: Action): Piece = {
    case BasicMove(target) => this.copy(coord=target, hasMoved=true)
    case Capture(victim) => this.copy(coord=victim.coord, hasMoved=true)
  }
}

final case class Pawn(color: Color, coord: Coord, hasMoved: Boolean)
    extends Piece with HasMoved {

  private val dir: Int = if (color == White) 1 else -1

  override def movement: Set[Coord] = {
    if (hasMoved) Set((coord._1 + dir, coord._2))
    else Set((coord._1 + dir, coord._2), (coord._1 + 2 * dir, coord._2))
  }

  def attacks: Set[Coord] = Set((coord._1 + dir, coord._2 - 1), (coord._1 + dir, coord._2 + 1))

  override def applyAction(action: Action): Piece = action match {
    // TODO: Promotion
    case BasicMove(target) => this.copy(coord=target, hasMoved=true)
    case Capture(victim) => this.copy(coord=victim.coord, hasMoved=true)
  }

  override def allActions(board: Board): Set[Action] = {
    val basicMoves = movement.filter(board.unoccupied).map(c => BasicMove(c))
    val captures = attacks.filter(board.occupied).map(c => Capture(board(c)))

    basicMoves ++ captures
  }

  override def canMoveTo(target: Coord, board: Board): Boolean = {
    movement.contains(target)
  }

  override def canAttack(target: Coord, board: Board): Boolean = {
    attacks.contains(target)
  }

  override def canCapture(that: Piece, board: Board): Boolean = {
    // TODO: en passant
    this.color != that.color && canAttack(that.coord, board)
  }
}
