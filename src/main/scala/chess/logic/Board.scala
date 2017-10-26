package chess
package logic

case class Board(board: Map[Coord, Piece]) {
  private def occupied(c: Coord): Boolean = board.contains(c)
  private def in_bds(c: Coord): Boolean =
    c._1 >= 1 && c._1 <= 8 && c._2 >= 1 && c._2 <= 8
  private def valid_target(target: Coord, attacker: Color): Boolean =
    in_bds(target) && (!occupied(target) || board(target).color == attacker.opp)

  def moves_of(p: Piece, coord: Coord): Vector[Coord] = p match {
    case Pawn(color, moved) =>
      val one_step = coord + (color.dir, 0)
      val two_steps = coord + (2 * color.dir, 0)

      if (occupied(one_step) || !in_bds(one_step))
        Vector()
      else if (moved || occupied(two_steps) || !in_bds(two_steps))
        Vector(one_step)
      else
        Vector(one_step, two_steps)

    // Non-pawns move the same way they attack
    case _ => targets_of(p, coord).filter(!occupied(_))
  }

  def targets_of(p: Piece, coord: Coord): Vector[Coord] = p match {
    case Pawn(color, _) =>
      val left = coord + (color.dir, -1)
      val right = coord + (color.dir, 1)
      Vector(left, right).filter(valid_target(_, color))

    case Rook(color, _) =>
      span_row(coord, color) ++ span_col(coord, color)

    case Knight(color) =>
      val jumps = for {
        a <- Vector((1, 2), (2, 1))
        (mag_r, mag_c) = a
        sgn_r <- Vector(-1, 1)
        sgn_c <- Vector(-1, 1)
      } yield coord + (mag_r * sgn_r, mag_c * sgn_c)
      jumps.filter(valid_target(_, color))

    case Bishop(color) =>
      span_rising_diag(coord, color) ++ span_falling_diag(coord, color)

    case Queen(color) =>
      span_row(coord, color) ++ span_col(coord, color) ++
        span_rising_diag(coord, color) ++ span_falling_diag(coord, color)

    case King(color, _) =>
      val nbhd =
        (for (x <- -1 to 1; y <- -1 to 1; if (x, y) != (0, 0))
          yield coord + (x, y)).toVector
      nbhd.filter(valid_target(_, color))
  }

  /** A vector (in the [[scala.collection.immutable.Vector]] sense <em>and</em> the mathematical sense)
    *   of coordinates that start from a given coordinate (exclusive) with given step sizes,
    *   taken until a coordinate is occupied or goes off the board. If the "last" coordinate is occupied,
    *   we can optionally choose to include it or not depending on the given color.
    * @param coord a starting coord <code>(r, c)</code>
    * @param dr row step size
    * @param dc col step size
    * @param excluded_color the color that is excluded
    * @return A vector of the coords:<br>
    * <code>(r+dr,c+dc), (r+2*dr,c+2*dc), (r+3*dr,c+3*dc), ... </code>
    * <br><br>
    * The vector stops when it encounters either:<ul>
    *   <li>a coord that is out of bounds, OR</li>
    *   <li>an occupied coord (includes or excludes this point based on <code>excluded_color</code>)</li></ul>
    */
  private def finite_coords_ray(coord: Coord, dr: Int, dc: Int, excluded_color: Color): Vector[Coord] =
    Iterator
      .iterate(coord)(_ + (dr, dc))
      .drop(1)
      .takeWhile(valid_target(_, attacker=excluded_color))
      .toVector

  private def span_row(coord: Coord, excluded_color: Color): Vector[Coord] = {
    val right = finite_coords_ray(coord, 0, 1, excluded_color)
    val left = finite_coords_ray(coord, 0, -1, excluded_color)
    left ++ right
  }
  private def span_col(coord: Coord, excluded_color: Color): Vector[Coord] = {
    val up = finite_coords_ray(coord, 1, 0, excluded_color)
    val down = finite_coords_ray(coord, -1, 0, excluded_color)
    up ++ down
  }
  private def span_rising_diag(coord: Coord, excluded_color: Color): Vector[Coord] = {
    val ne = finite_coords_ray(coord, 1, 1, excluded_color)
    val sw = finite_coords_ray(coord, -1, -1, excluded_color)
    ne ++ sw
  }
  private def span_falling_diag(coord: Coord, excluded_color: Color): Vector[Coord] = {
    val nw = finite_coords_ray(coord, 1, -1, excluded_color)
    val se = finite_coords_ray(coord, -1, 1, excluded_color)
    nw ++ se
  }

}

object Board {
  val INITIAL = Board {
    val pawns = (1 to 8).flatMap{
      col => List(
        ((2, col), Pawn(White, moved=false)),
        ((7, col), Pawn(Black, moved=false)))
    }.toMap

    val rooks: Map[Coord, Piece] = {
      val wr = Rook(White, moved=false)
      val br = Rook(Black, moved=false)
      Map((1, 1) -> wr, (1, 8) -> wr, (8, 1) -> br, (8, 8) -> br)
    }

    val knights: Map[Coord, Piece] = {
      val wk = Knight(White)
      val bk = Knight(Black)
      Map((1, 2) -> wk, (1, 7) -> wk, (8, 2) -> bk, (8, 7) -> bk)
    }

    val bishops: Map[Coord, Piece] = {
      val wb = Bishop(White)
      val bb = Bishop(Black)
      Map((1, 3) -> wb, (1, 6) -> wb, (8, 3) -> bb, (8, 6) -> bb)
    }

    val royalty = Map(
      (1, 4) -> Queen(White), (1, 5) -> King(White, moved=false),
      (8, 4) -> Queen(Black), (8, 5) -> King(Black, moved=false))

    pawns ++ rooks ++ knights ++ bishops ++ royalty
  }
}