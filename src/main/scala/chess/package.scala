package object chess {
  implicit class InStuff[T](val x: T) extends AnyVal {
    def in(coll: Seq[T]): Boolean = coll.contains(x)
    def in(coll: Set[T]): Boolean = coll.contains(x)
    def in(coll: Map[T, _]): Boolean = coll.contains(x)

    def notIn(coll: Seq[T]): Boolean = !coll.contains(x)
    def notIn(coll: Set[T]): Boolean = !coll.contains(x)
    def notIn(coll: Map[T, _]): Boolean = !coll.contains(x)
  }

  /**
    * (Row/Rank, Column/File), each from 1 to 8
    * White takes rows 1 and 2. Black takes rows 3 and 4
    */
  type Coord = (Int, Int)

}
