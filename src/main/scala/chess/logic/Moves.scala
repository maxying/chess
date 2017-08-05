package chess
package logic

object Moves {

  def sameRow(c1: Coord, c2: Coord): Boolean = c1._1 == c2._2

  def sameCol(c1: Coord, c2: Coord): Boolean = c1._2 == c2._2

  def sameDiag(c1: Coord, c2: Coord): Boolean = math.abs(c1._1 - c2._1) == math.abs(c1._2 - c2._2)

  def ellShaped(c1: Coord, c2: Coord): Boolean = {
    (math.abs(c1._1 - c2._1) == 1 && math.abs(c1._2 - c2._2) == 2) ||
      (math.abs(c1._1 - c2._1) == 2 && math.abs(c1._2 - c2._2) == 1)
  }

  def clearPath(c1: Coord, c2: Coord, occupancy: Map[Coord, Piece]): Boolean = {
    if (sameRow(c1, c2)) {
      val lo = math.min(c1._2, c2._2) + 1
      val hi = math.max(c1._2, c2._2) - 1
      (lo to hi).forall(col => (c1._1, col) notIn occupancy)
    } else if (sameCol(c1, c2)) {
      val lo = math.min(c1._1, c2._1) + 1
      val hi = math.max(c1._1, c2._1) - 1
      (lo to hi).forall(row => (row, c1._1) notIn occupancy)
    } else if (sameDiag(c1, c2)) {
      val stepX = math.signum(c2._1 - c1._1)
      val stepY = math.signum(c2._2 - c1._2)
      val steps = math.abs(c1._1 - c2._1) - 1
      (1 to steps).forall(_ => (c1._1 + stepX, c1._2 + stepY) notIn occupancy)
    } else false
  }

}
