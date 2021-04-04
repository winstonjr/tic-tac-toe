package boardgame

import scala.collection.immutable.HashMap
import scala.io.StdIn.readInt
import scala.util.control.Breaks._

object Game extends App {
  //  print("Nome do Jogador 1: ")
  //  val jogador1 = readLine()
  //  print("Nome do Jogados 2: ")
  //  val jogador2 = readLine()

    val jogador1 = "Winner"
    val jogador2 = "Looser"

    val ticTacToeMatch = Game(jogador1, jogador2)

    do {
      ticTacToeMatch.play() match {
        case Left(bla) => println(bla)
        case default => default
      }

      ticTacToeMatch.printBoard()
    } while (ticTacToeMatch.boardStatus match {
      case Left(_) => true
      case Right(_) => false
    })

    println(ticTacToeMatch.boardStatus)
}

case class Game(playerXName: String, playerOName: String) {
  private val playerX: PlayerBoard = ComputerPlayer("X", playerXName)
  private val playerO: PlayerBoard = HumanPlayer("O", playerOName)
//  private val playerX: PlayerBoard = HumanPlayer("X", playerXName)
//  private val playerO: PlayerBoard = ComputerPlayer("O", playerOName)
  private var turn: PlayerBoard = playerX
  private val board: Board = Board(playerX, playerO)

  def turnName: String = turn.name

  def boardStatus: Either[String, String] = board.boardStatus()

  def printBoard(): Unit = board.printBoard()

  def play(): Either[String, String] = {
    val positions = turn.getMove(board.mergePlayersBoard)

    board.stamp(turn.symbol, positions._1, positions._2) match {
      case Left(bla) => Left(bla)
      case Right(ble) =>
        turn = if (turn.equals(playerO)) playerX else playerO
        Right(ble)
    }
  }
}

trait PlayerBoard {
  val symbol: String
  val name: String
  final val defaultEmpty = 0
  protected final val boardSize = 3
  final val board: Array[Int] = Array.fill(boardSize * boardSize)(defaultEmpty)

  final def isWinner: Boolean = {
    this.isWinner(this.board)
  }

  /**
   * validates if is a winning board
   *
   * @param thisBoard board to be validated
   * @return true in case of a winning board, false if it is not winning board
   */
  final def isWinner(thisBoard: Array[Int]): Boolean = {
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
      (Integer.parseInt(thisBoard.mkString(""), 2) & v) == v
    })

    // https://loganmeetsworld.com/2017/10/15/using-of-bitwise-operators.html
    // https://github.com/loganmeetsworld/js-tic-tac-toe/blob/lm/master/tic-tac-toe.js
  }

  final def isInsideBoardBoundaries(row: Int, col: Int): Boolean =
    row > boardSize || row < 1 || col > boardSize || col < 1

  final def isPositionFree(row: Int, col: Int): Boolean =
    defaultEmpty.equals(board(getCalculatedArrayPosition(row, col)))

  final def getCalculatedArrayPosition(row: Int, col: Int): Int =
    if (row == 1) row - 1 + col - 1 else if (row == 2) row + col else row + 1 + col + 1

  final def setPlay(row: Int, col: Int): Unit = {
    val arrayPos = getCalculatedArrayPosition(row, col)
    board(arrayPos) = 1
  }

  final def getBoardArray: Array[String] =
    board.map(v => if (1.equals(v)) symbol else " ")

  def getMove(board: Array[String]): (Int, Int) = {
    print(s"$name escolha a linha: ")
    val row = readInt()
    print(s"$name escolha a Coluna: ")
    val col = readInt()

    (row, col)
  }
}

case class HumanPlayer(symbol: String, name: String) extends PlayerBoard {
}

case class ComputerPlayer(symbol: String, name: String) extends PlayerBoard {
  private val xString = "X"
  private val oString = "O"
  private val spaceString = " "
  private val opponentSymbol = if (xString.equals(symbol)) oString else xString

  private def winnerTieOrNull(board: Array[String]): String = {
    val boardX = board.map(value => if (xString.equals(value)) 1 else 0)
    val boardO = board.map(value => if (oString.equals(value)) 1 else 0)

    if (this.isWinner(boardX)) {
      xString
    } else if (this.isWinner(boardO)) {
      oString
    } else if (9.equals(board.count(v => xString.equals(v) || oString.equals(v)))) {
      "tie"
    } else {
      ""
    }
  }

  private val scores: HashMap[String, Int] = HashMap(xString -> 1, oString -> -1, "tie" -> 0)

  private def minimax(board: Array[String], depth: Int, isMaximizing: Boolean): Int = {
    // baseado na função apresentada aqui: https://www.youtube.com/watch?v=trKjYdBASyQ
    val boardTotal = board.count(p => xString.equals(p) || oString.equals(p))
    val gameState = winnerTieOrNull(board)
    if (1.equals(boardTotal) && symbol.equals(board(4))) {
      1_000_000_000
    } else if (0.equals(depth) && symbol.equals(gameState)) {
      999_999_000
    } else {
      if (gameState.nonEmpty) {
        return scores(gameState)
      }
      val func = (initVal: Int, symb: String, optFunc: (Int, Int) => Int) => {
        var bestVal: Int = initVal
        for (index <- 0 to 8) {
          if (spaceString.equals(board(index))) {
            board(index) = symb
            bestVal = optFunc(bestVal, minimax(board, depth + 1, !isMaximizing))
            board(index) = spaceString
          }
        }
        bestVal
      }

      if (isMaximizing) {
        func(Int.MinValue, symbol, Math.max)
      } else {
        func(Int.MaxValue, opponentSymbol, Math.min)
      }
    }
  }

  override def getMove(board: Array[String]): (Int, Int) = {
    var bestMove = Int.MinValue
    var moveIndex = Int.MinValue
    breakable { for (index <- 0 to 8) {
      if (spaceString.equals(board(index))) {
        board(index) = symbol
        val score = minimax(board, depth = 0, isMaximizing = false)
        board(index) = spaceString
        if (score > bestMove) {
          println(s"score: $score, index: $index")
          bestMove = score
          moveIndex = index

          if (1_000_000_000.equals(score) || 999_999_000.equals(score)) {
            break
          }
        }
      }
    }}

    val positions = Array(
      (1, 1), (1, 2), (1, 3),
      (2, 1), (2, 2), (2, 3),
      (3, 1), (3, 2), (3, 3))

    positions(moveIndex)
  }
}

case class Board(playerX: PlayerBoard, playerO: PlayerBoard) {
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

  private def setBoardPlay(symbol: String, row: Int, col: Int): Unit =
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
    if (playerX.isWinner) {
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
   * merge the board of the two players using the player symbol
   *
   * @return board merged
   */
  def mergePlayersBoard: Array[String] = {
    (playerX.getBoardArray zip playerO.getBoardArray).map(v => {
      if ("X".equals(v._1)) "X" else if ("O".equals(v._2)) "O" else " "
    })
  }

  /**
   * Prints the board on console
   */
  def printBoard(): Unit = {
    val DIMENSION = 3
    val mergeBoard: Array[String] = mergePlayersBoard

    println("####################")
    println((1 to DIMENSION).map(row =>
      (1 to DIMENSION).map(col => {
        val position = playerX.getCalculatedArrayPosition(row, col)
        s" ${mergeBoard(position)} "
      }).mkString("|")).mkString("\n---+---+---\n") + "\n")
    println("####################")
  }
}
