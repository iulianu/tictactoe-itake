import BoardConstants._
import scala.annotation.tailrec
import scala.util.{Success, Try}

object Game extends App {
  /**
   * Add an extension method on the Board class.
   * But keep it separate from the Board source code, since
   * it doesn't belong to its core functionality.
   */
  implicit class BoardPrettyPrinter(board: Board) {
    def prettyPrint {
      for(row <- 0 until Size) {
        if( row >  0)
          println("----|-----|----")
        for(col <- 0 until Size) {
          if(col > 0) print(" | ")
          val position = positionFor(row, col)
          board.squareAt(position) match {
            case Empty =>
              print(s"<$position>")
            case mark: PlayerMark =>
              print(" " + mark.markAsString + " ")
          }
        }
        println
      }
    }
  }

  /**
   * Main function. Reads a play from the keyboard,
   * applies it to the board, checks for winners,
   * then calls itself in a tail-recursive manner.
   */
  @tailrec
  def playMoveRec(currentBoard: Board, currentTurn: PlayerMark) {
    currentBoard.prettyPrint
    print(s"Player ${currentTurn.markAsString}, where are you playing? Type a number and press Enter.  ")

    val playResult = for {
      // Try to read a position from the keyboard
      position <- Try(Integer.parseInt(readLine())).toOption
      // Try to play on the board
      newBoard <- currentBoard.play(position, currentTurn)
      // If successful so far, pass the turn to the opponent
      nextTurn <- Some(PlayerMark.opponentOf(currentTurn))
    } yield (newBoard, nextTurn)

    playResult match {
      case Some((board, nextTurn)) =>
        // Play was valid
        board.winner match {
          case Some(player) =>
            // We've got a wiener!
            board.prettyPrint
            println(s"!!!! ${player.markAsString} wins! Well done!")
            playMoveRec(emptyBoard, nextTurn)
          case None =>
            playMoveRec(board, nextTurn)
        }
      case None =>
        playMoveRec(currentBoard, currentTurn)
    }
  }

  /* Entry point */
  val emptyBoard = Board()
  val firstMover = X
  println("Starting Tic-Tac-Toe. Press Ctrl-C to finish.")
  playMoveRec(emptyBoard, firstMover)

}
