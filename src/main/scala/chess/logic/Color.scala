package chess.logic

sealed trait Color { def opp: Color; def dir: Int }
  case object White extends Color { def opp: Color = Black; def dir: Int = 1 }
  case object Black extends Color { def opp: Color = White; def dir: Int = -1 }
