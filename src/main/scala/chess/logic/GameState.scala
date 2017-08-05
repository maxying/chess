package chess.logic

case class GameState(board: Board, turn: Color, captured: Map[Piece, Int], history: List[Board]) {
  import GameState._

  def updateGraveyard(piece: Piece): Map[Piece, Int] = piece match {
    case Pawn(White, _, _) => captured.updated(DEAD_PAWN_WHITE, captured(DEAD_PAWN_WHITE) + 1)
    case Pawn(Black, _, _) => captured.updated(DEAD_PAWN_BLACK, captured(DEAD_PAWN_BLACK) + 1)
    case Rook(White, _, _) => captured.updated(DEAD_ROOK_WHITE, captured(DEAD_ROOK_WHITE) + 1)
    case Rook(Black, _, _) => captured.updated(DEAD_ROOK_BLACK, captured(DEAD_ROOK_BLACK) + 1)
    case Knight(White, _) => captured.updated(DEAD_KNIGHT_WHITE, captured(DEAD_KNIGHT_WHITE) + 1)
    case Knight(Black, _) => captured.updated(DEAD_KNIGHT_BLACK, captured(DEAD_KNIGHT_BLACK) + 1)
    case Bishop(White, _) => captured.updated(DEAD_BISHOP_WHITE, captured(DEAD_BISHOP_WHITE) + 1)
    case Bishop(Black, _) => captured.updated(DEAD_BISHOP_BLACK, captured(DEAD_BISHOP_BLACK) + 1)
    case Queen(White, _) => captured.updated(DEAD_QUEEN_WHITE, captured(DEAD_QUEEN_WHITE) + 1)
    case Queen(Black, _) => captured.updated(DEAD_QUEEN_BLACK, captured(DEAD_QUEEN_BLACK) + 1)
  }
}

object GameState {
  val DEAD_PAWN_WHITE = Pawn(White, (0, 0), false)
  val DEAD_PAWN_BLACK = Pawn(Black, (0, 0), false)
  val DEAD_ROOK_WHITE = Rook(White, (0, 0), false)
  val DEAD_ROOK_BLACK = Rook(Black, (0, 0), false)
  val DEAD_KNIGHT_WHITE = Knight(White, (0, 0))
  val DEAD_KNIGHT_BLACK = Knight(Black, (0, 0))
  val DEAD_BISHOP_WHITE = Bishop(White, (0, 0))
  val DEAD_BISHOP_BLACK = Bishop(Black, (0, 0))
  val DEAD_QUEEN_WHITE = Queen(White, (0, 0))
  val DEAD_QUEEN_BLACK = Queen(Black, (0, 0))

  val INIT: GameState = GameState(Board.INIT, White, Map.empty, List.empty)
}
