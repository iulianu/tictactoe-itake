
sealed abstract class SquareValue
final case object Empty extends SquareValue
sealed abstract class PlayerMark extends SquareValue {
  def markAsString: String
}
final case object X extends PlayerMark {
  val markAsString = "X"
}
final case object O extends PlayerMark {
  val markAsString = "O"
}

object PlayerMark {
  def opponentOf(player: PlayerMark) = if(player == X) { O } else { X }
}

case class Board(values: Seq[SquareValue] = Seq.fill(BoardConstants.Size * BoardConstants.Size)(Empty)) {

  def squareAt(position: Int): SquareValue =
    values(position)

  def squareFree(position: Int) =
    squareAt(position) == Empty

  /**
   * Returns a new Board with the mark played on it, or None if the play is impossible
   */
  def play(position: Int, mark: PlayerMark): Option[Board] =
    if(BoardConstants.validPosition(position) && squareFree(position))
      Some(Board(values.patch(position, List(mark), 1)))
    else
      None

  /**
   * Who is winning this board. Returns an Option[PlayerMark].
   * If anyone is winning, the relevant player mark will be returned
   * in a Some(). If no one is winning, the return value is None.
   *
   * @throws IllegalArgumentException if the play is impossible
   */
  def winner: Option[PlayerMark] = horizontalWinner orElse
                                   verticalWinner orElse
                                   diagonalWinner

  private final val NoWinner = Option.empty[PlayerMark]

  /** Who wins on the horizontal? */
  def horizontalWinner = BoardConstants.Range.foldLeft(NoWinner) {
    (winnerSoFar, row) => winnerSoFar.orElse(winnerOnRow(row))
  }

  /** Who wins on the vertical? */
  def verticalWinner = BoardConstants.Range.foldLeft(NoWinner) {
    (winnerSoFar, col) => winnerSoFar.orElse(winnerOnColumn(col))
  }

  /** Who wins on the diagonals? */
  def diagonalWinner =
    diagonal1Winner orElse diagonal2Winner

  /** Who wins on the given row? */
  def winnerOnRow(row: Int) =
    winnerOnPositions(BoardConstants.positionsForRow(row))

  /** Who wins on the given columns? */
  def winnerOnColumn(col: Int) =
    winnerOnPositions(BoardConstants.positionsForCol(col))

  def diagonal1Winner =
    winnerOnPositions(BoardConstants.positionsForDiag1)

  def diagonal2Winner =
    winnerOnPositions(BoardConstants.positionsForDiag2)

  /**
   * If all positions have non-Empty marks, and all the marks
   * belong to the same player, then the player is a winner
   */
  def winnerOnPositions(positions: Seq[Int]): Option[PlayerMark] = {
    if(positions.size != BoardConstants.Size) /* For sanity check */
      None
    else {
      squareAt(positions.head) match {
        case Empty => // No mark in the first position
          None
        case playerMark: PlayerMark =>
          if( positions.forall { squareAt(_) == playerMark })
            Some(playerMark)
          else
            None
      }
    }
  }

}

object BoardConstants {
  final val Size = 3
  final val Range = (0 until Size)

  def validPosition(position: Int) =
    position >= 0 && position < Size * Size

  def positionFor(row: Int, col: Int) =
    row * Size + col

  /* What positions (0 until 8) match the given row on the board */
  def positionsForRow(row: Int): Seq[Int] =
    ((row * Size) until (row + 1) * Size).toList

  /* What positions (0 until 8) match the given column on the board */
  def positionsForCol(col: Int): Seq[Int] =
    Range.map { row => row * Size + col }

  /* What positions (0 until 8) match the 1st diagonal of the board */
  def positionsForDiag1: Seq[Int] =
    Range.map { idx => idx * Size + idx }

  /* What positions (0 until 8) match the 2nd diagonal of the board */
  def positionsForDiag2: Seq[Int] =
    Range.map { idx => (idx + 1) * Size - idx - 1 }
}