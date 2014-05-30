import org.specs2.mutable._

class BoardSpec extends Specification {

  "empty board" should {
    "have nothing" in {
      Board().squareFree(0) should beTrue
    }
  }

  "board" should {
    "not be free if I play something at a position" in {
      val board = Board().play(0, X).get
      board.squareFree(0) should beFalse
    }

    "accept a piece" in {
      val board = Board().play(0, X).get
      board.squareAt(0) should beEqualTo(X)
    }

    "not accept a play on the same position" in {
      val board = Board().play(0, X).get
      board.play(0, O) should beNone
    }

    "not be a winner" in {
      val nonWinningBoard = Board(List(X, O, X,
                                       O, X, O,
                                       O, X, O))
      nonWinningBoard.winner should beNone
    }

    "be a win on the horizontal" in {
      val winningBoard = Board(List(X,    Empty, Empty,
                                    O,    O,     O,
                                    X,    O,     X))
      winningBoard.winner should beSome(O)
    }

    "be a win on the vertical" in {
      val winningBoard = Board(List(X,    O,     Empty,
                                    X,    Empty, Empty,
                                    X,    O,     X))
      winningBoard.winner should beSome(X)
    }

    "be a win on diagonal 1" in {
      val winningBoard = Board(List(X,     Empty, Empty,
                                    Empty, X,     O,
                                    X,     O,     X))
      winningBoard.winner should beSome(X)
    }

    "be a win on diagonal 2" in {
      val winningBoard = Board(List(Empty, Empty, X,
                                    Empty, X,     O,
                                    X,     O,     X))
      winningBoard.winner should beSome(X)
    }
  }

  "board indexes" should {

    //  0   |   1   |   2
    //  ----|-------|-------
    //  3   |   4   |   5
    //  ----|-------|-------
    //  6   |   7   |   8

    "be correct for a row" in {
      BoardConstants.positionsForRow(0) should containTheSameElementsAs(List(0, 1, 2))
    }
    "be correct for a column" in {
      BoardConstants.positionsForCol(0) should containTheSameElementsAs(List(0, 3, 6))
    }
    "be correct for the 1st diagonal" in {
      BoardConstants.positionsForDiag1 should containTheSameElementsAs(List(0, 4, 8))
    }
    "be correct for the 2nd diagonal" in {
      BoardConstants.positionsForDiag2 should containTheSameElementsAs(List(2, 4, 6))
    }

    "be valid" in {
      BoardConstants.validPosition(0) should beTrue
    }
    "be too small" in {
      BoardConstants.validPosition(-1) should beFalse
    }
    "be too large" in {
      BoardConstants.validPosition(9) should beFalse
    }

  }

  "opponents" should {
    "yield the opponent of X" in {
      PlayerMark.opponentOf(X) should be(O)
    }
    "yield the opponent of O" in {
      PlayerMark.opponentOf(O) should be(X)
    }
  }
}
