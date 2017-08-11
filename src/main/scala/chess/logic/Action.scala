package chess
package logic

sealed trait Action

final case class BasicMove(to: Coord) extends Action
final case class Capture(victim: Piece) extends Action
//final case class Castle(king: Piece, rook: Piece) extends Action

object Action {

  def coordsInRow(coord: Coord): Set[Coord] = {
    (for (col <- 1 to 8; if col != coord._2) yield (coord._1, col)).toSet
  }

  def coordsInCol(coord: Coord): Set[Coord] = {
    (for (row <- 1 to 8; if row != coord._1) yield (row, coord._2)).toSet
  }

  def coordsInDiags(coord: Coord): Set[Coord] = {
    def offsets: Seq[Coord] = {
      (1 to 7).zip(1 to 7) ++
        (1 to 7).zip(-1 to -7) ++
        (-1 to -7).zip(1 to 7) ++
        (-1 to -7).zip(-1 to -7)
    }

    (for {
      (dx, dy) <- offsets
      if inBounds((coord._1 + dx, coord._2 + dy))
    } yield (coord._1 + dx, coord._2 + dy)).toSet
  }

  def coordsInEll(coord: Coord): Set[Coord] = {
    def offsets = Set((1, 2), (1, -2), (2, 1), (2, -1), (-1, 2), (-1, -2), (-2, 1), (-2, -1))
    for {
      (dx, dy) <- offsets
      if inBounds((coord._1 + dx, coord._2 + dy))
    } yield (coord._1 + dx, coord._2 + dy)
  }

  def inBounds(coord: Coord): Boolean = (1 to 8).contains(coord._1) && (1 to 8).contains(coord._2)

  def inSameRow(c1: Coord, c2: Coord): Boolean = c1._1 == c2._2

  def inSameCol(c1: Coord, c2: Coord): Boolean = c1._2 == c2._2

  def inSameDiag(c1: Coord, c2: Coord): Boolean = math.abs(c1._1 - c2._1) == math.abs(c1._2 - c2._2)

  def ellShaped(c1: Coord, c2: Coord): Boolean = {
    (math.abs(c1._1 - c2._1) == 1 && math.abs(c1._2 - c2._2) == 2) ||
      (math.abs(c1._1 - c2._1) == 2 && math.abs(c1._2 - c2._2) == 1)
  }

  def clearPath(c1: Coord, c2: Coord, board: Board): Boolean = {
    if (inSameRow(c1, c2)) {
      val lo = math.min(c1._2, c2._2) + 1
      val hi = math.max(c1._2, c2._2) - 1
      (lo to hi).forall(col => !board.self.contains((c1._1, col)))
    } else if (inSameCol(c1, c2)) {
      val lo = math.min(c1._1, c2._1) + 1
      val hi = math.max(c1._1, c2._1) - 1
      (lo to hi).forall(row => !board.self.contains((row, c1._1)))
    } else if (inSameDiag(c1, c2)) {
      val stepX = math.signum(c2._1 - c1._1)
      val stepY = math.signum(c2._2 - c1._2)
      val steps = math.abs(c1._1 - c2._1) - 1
      (1 to steps).forall(_ => !board.self.contains((c1._1 + stepX, c1._2 + stepY)))
    } else false
  }

}
