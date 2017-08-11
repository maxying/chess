package chess.logic

sealed trait Color { def opp: Color }
case object White extends Color { def opp: Color = Black }
case object Black extends Color { def opp: Color = White }
