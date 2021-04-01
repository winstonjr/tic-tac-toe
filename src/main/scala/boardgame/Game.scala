package boardgame

import scala.io.StdIn.readInt

object Game extends App {
  //  print("Nome do Jogador 1: ")
  //  val jogador1 = readLine()
  //  print("Nome do Jogados 2: ")
  //  val jogador2 = readLine()
  val jogador1 = "Winner"
  val jogador2 = "Looser"

  val ticTacToeMatch = Game(jogador1, jogador2)

  do {
    ticTacToeMatch.printBoard()

    print(s"${ticTacToeMatch.turnName} escolha a linha: ")
    val row = readInt()
    print(s"${ticTacToeMatch.turnName} escolha a Coluna: ")
    val col = readInt()

    ticTacToeMatch.play(row, col) match {
      case Left(bla) => println(bla)
      case default => default
    }
  } while (ticTacToeMatch.boardStatus match {
    case Left(_) => true
    case Right(_) => false
  })

  println(ticTacToeMatch.boardStatus)}

case class Game(playerXName: String, playerOName: String) {
  private val playerX = Player("X", playerXName)
  private val playerO = Player("O", playerOName)
  private var turn: Player = playerX
  private val board: Board = Board(playerX, playerO)

  def turnName : String = turn.name
  def boardStatus: Either[String, String] = board.boardStatus()
  def printBoard(): Unit = board.printBoard()

  def play(row: Int, col: Int): Either[String, String] = {
    board.stamp(turn.symbol, row, col) match {
      case Left(bla) => Left(bla)
      case Right(ble) =>
        turn = if (turn.equals(playerO)) playerX else playerO
        Right(ble)
    }
  }
}

trait PlayerBoard {
  val board: Array[Int]
  val symbol: String
  final val defaultEmpty = 0
  protected final val boardSize = 3

  /**
   * validates if is a winning board
   *
   * @param board board to be validated
   * @return true in case of a winning board, false if it is not winning board
   */
  def isWinner: Boolean = {
    // 000000111 = 7   = Row 3
    // 000111000 = 56  = Row 2
    // 111000000 = 448 = Row 1
    // 001001001 = 73  = Column 3
    // 010010010 = 146 = Column 2
    // 100100100 = 292 = Column 1
    // 100010001 = 273 = Diagonal
    // 001010100 = 84  = Inverse Diagonal
    val winningPlays: Seq[Int] = Seq(7, 56, 448, 73, 146, 292, 273, 84)

    winningPlays.exists(v => {
      (Integer.parseInt(board.mkString(""), 2) & v) == v
    })

    // https://loganmeetsworld.com/2017/10/15/using-of-bitwise-operators.html
    // https://github.com/loganmeetsworld/js-tic-tac-toe/blob/lm/master/tic-tac-toe.js
  }

  def isInsideBoardBoundaries(row: Int, col: Int): Boolean =
    row > boardSize || row < 1 || col > boardSize || col < 1

  def isPositionFree(row: Int, col: Int): Boolean =
    defaultEmpty.equals(board(getCalculatedArrayPosition(row, col)))

  private def getCalculatedArrayPosition(row: Int, col: Int): Int =
    if (row == 1) row-1 + col-1 else if (row == 2) row + col else row+1 + col+1

  def setPlay(row: Int, col: Int) : Unit = {
    val arrayPos = getCalculatedArrayPosition(row, col)
    board(arrayPos) = 1
  }

  def getBoardArray: Array[String] =
    board.map(v => if (1.equals(v)) symbol else " ")
}

case class Player(symbol: String, name: String) extends PlayerBoard {
  val board: Array[Int] = Array.fill(boardSize*boardSize)(defaultEmpty)
}

case class Board(playerX: Player, playerO: Player) {
  def stamp(symbol: String, row: Int, col: Int): Either[String, String] = {
    if (playerX.isInsideBoardBoundaries(row, col)) {
      Left("a posição não existe no tabuleiro")
    } else {
      if (isPositionFree(row, col)) {
        setBoardPlay(symbol, row, col)

        Right("jogada registrada no tabuleiro")
      } else {
        Left("a posição não está disponível para ser jogada, repita")
      }
    }
  }

  private def isPositionFree(row: Int, col: Int): Boolean =
    playerX.isPositionFree(row, col) && playerO.isPositionFree(row, col)
  private def setBoardPlay(symbol: String, row: Int, col: Int) : Unit =
    if (playerX.symbol.equals(symbol)) playerX.setPlay(row, col) else playerO.setPlay(row, col)

  /**
   * Validates if the game is a Draw
   *
   * @return true in case of a draw, false if it is not the case
   */
  private def isGameDraw: Boolean =
    9.equals(playerX.board.count(v => 1.equals(v)) + playerO.board.count(v => 1.equals(v)))

  /**
   * Validates if the game keeps going or stop
   *
   * @return Left if the game continue, Right if the game has ended
   */
  def boardStatus(): Either[String, String] = {
    if (playerX.isWinner)  {
      Right(s"Jogador ${playerX.name} (${playerX.symbol}) venceu")
    } else if (playerO.isWinner) {
      Right(s"Jogador ${playerO.name} (${playerO.symbol}) Venceu")
    } else if (isGameDraw) {
      Right("Jogo empatado")
    } else {
      Left("Jogo continua")
    }
  }

  /**
   * Prints the board on console
   */
  def printBoard(): Unit = {
    val DIMENSION = 3
    val mergeBoard: Array[String] = (playerX.getBoardArray zip playerO.getBoardArray).map(v => {
      if ("X".equals(v._1)) "X" else if ("O".equals(v._2)) "O" else " "
    })

    println()
    println((1 to DIMENSION).map(row =>
      (1 to DIMENSION).map(col => {
        val position = if (row == 1) row-1 + col-1 else if (row == 2) row + col else row+1 + col+1
        s" ${mergeBoard(position)} "
      }).mkString("|")).mkString("\n---+---+---\n") + "\n")
    println()
  }


  //  private def isWinningBoard(board: Array[Int]): Boolean = {
  //    // 000000111 = 7   = Row 3
  //    // 000111000 = 56  = Row 2
  //    // 111000000 = 448 = Row 1
  //    // 001001001 = 73  = Column 3
  //    // 010010010 = 146 = Column 2
  //    // 100100100 = 292 = Column 1
  //    // 100010001 = 273 = Diagonal
  //    // 001010100 = 84  = Inverse Diagonal
  //    val winningPlays: Seq[Int] = Seq(7, 56, 448, 73, 146, 292, 273, 84)
  //
  //    winningPlays.exists(v => {
  //      (Integer.parseInt(board.mkString(""), 2) & v) == v
  //    })
  //
  //    // https://loganmeetsworld.com/2017/10/15/using-of-bitwise-operators.html
  //    // https://github.com/loganmeetsworld/js-tic-tac-toe/blob/lm/master/tic-tac-toe.js
  //  }

  //  val testBoards: Array[Array[Int]] = Array(
  //    // Straight Win
  //    Array[Int](0, 0, 0, 0, 0, 0, 1, 1, 1),
  //    Array[Int](0, 0, 0, 1, 1, 1, 0, 0, 0),
  //    Array[Int](1, 1, 1, 0, 0, 0, 0, 0, 0),
  //    Array[Int](0, 0, 1, 0, 0, 1, 0, 0, 1),
  //    Array[Int](0, 1, 0, 0, 1, 0, 0, 1, 0),
  //    Array[Int](1, 0, 0, 1, 0, 0, 1, 0, 0),
  //    Array[Int](1, 0, 0, 0, 1, 0, 0, 0, 1),
  //    Array[Int](0, 0, 1, 0, 1, 0, 1, 0, 0),
  //    // Win
  //    Array[Int](0, 1, 0, 0, 0, 0, 1, 1, 1),
  //    Array[Int](0, 0, 0, 1, 1, 1, 0, 0, 1),
  //    Array[Int](1, 1, 1, 0, 0, 1, 0, 0, 0),
  //    Array[Int](0, 0, 1, 1, 0, 1, 0, 0, 1),
  //    Array[Int](0, 1, 1, 0, 1, 0, 0, 1, 0),
  //    Array[Int](1, 0, 1, 1, 0, 0, 1, 0, 0),
  //    Array[Int](1, 0, 0, 0, 1, 0, 1, 0, 1),
  //    Array[Int](0, 0, 1, 0, 1, 0, 1, 0, 1),
  //    // loss
  //    Array[Int](1, 1, 0, 1, 0, 0, 0, 0, 0),
  //    Array[Int](0, 0, 0, 1, 0, 1, 0, 0, 0),
  //    Array[Int](1, 0, 1, 0, 0, 0, 0, 0, 0),
  //    Array[Int](0, 0, 1, 0, 0, 0, 0, 0, 1),
  //    Array[Int](0, 0, 0, 0, 1, 0, 0, 1, 0),
  //    Array[Int](0, 0, 0, 1, 0, 0, 1, 0, 0),
  //    Array[Int](1, 0, 0, 0, 1, 0, 0, 0, 0),
  //    Array[Int](0, 0, 1, 0, 1, 0, 0, 0, 0),
  //
  //    Array[Int](1, 0, 0, 0, 0, 0, 0, 0, 0)
  //  )
  //  //testBoards.foreach(board => println(s"Ganhou mizerávi? ${if (winningBoard(board)) "Sim =D" else "Não ='/"}"))
}
