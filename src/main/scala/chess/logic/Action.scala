package chess
package logic

sealed trait Action
  final case class Move(from: Coord, to: Coord) extends Action
  final case class Attack(from: Coord, to: Coord) extends Action