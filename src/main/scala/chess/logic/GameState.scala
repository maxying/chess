package chess
package logic


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

//noinspection NameBooleanParameters
object GameState {
  val GRAVE: Coord = (0, 0)
  val DEAD_PAWN_WHITE = Pawn(White, GRAVE, false)
  val DEAD_PAWN_BLACK = Pawn(Black, GRAVE, false)
  val DEAD_ROOK_WHITE = Rook(White, GRAVE, false)
  val DEAD_ROOK_BLACK = Rook(Black, GRAVE, false)
  val DEAD_KNIGHT_WHITE = Knight(White, GRAVE)
  val DEAD_KNIGHT_BLACK = Knight(Black, GRAVE)
  val DEAD_BISHOP_WHITE = Bishop(White, GRAVE)
  val DEAD_BISHOP_BLACK = Bishop(Black, GRAVE)
  val DEAD_QUEEN_WHITE = Queen(White, GRAVE)
  val DEAD_QUEEN_BLACK = Queen(Black, GRAVE)

  val INIT_GRAVEYARD: Map[Piece, Int] = Map(
    DEAD_PAWN_WHITE -> 0,
    DEAD_PAWN_BLACK -> 0,
    DEAD_ROOK_WHITE -> 0,
    DEAD_ROOK_BLACK -> 0,
    DEAD_KNIGHT_WHITE -> 0,
    DEAD_KNIGHT_BLACK -> 0,
    DEAD_BISHOP_WHITE -> 0,
    DEAD_BISHOP_BLACK -> 0,
    DEAD_QUEEN_WHITE -> 0,
    DEAD_QUEEN_BLACK -> 0
  )

  val INIT: GameState = GameState(Board.INIT, White, INIT_GRAVEYARD, List.empty)
}
