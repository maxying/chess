package object chess {
  /**
    * (Row/Rank, Column/File)<br>
    * White takes rows 1 and 2; Black takes rows 7 and 8
    */
  type Coord = (Int, Int)

  implicit class CoordAddition(lhs: Coord) {
    def +(rhs: Coord): Coord = (lhs._1 + rhs._1, lhs._2 + rhs._2)
  }

}
