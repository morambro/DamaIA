import scala.math._

object Player extends Enumeration {
  type Player= Value
  val White = Value("w")
  val Black = Value("b") 
}

import Player._

class Pawn(val player: Player) {

  override def equals(o: Any) = o match { case p: Pawn => player == p.player }

  def this(p: Pawn) { this(p.player) }
}

class KingPawn(player: Player) extends Pawn(player)

/**
  * logic square of the chessboard
  */
class Square(val x: Int, val y: Int) {

  var pColor         = "light"
  var pContent: Pawn = null

  def color = pColor
  def color_=(c: String) { pColor = c }

  def content = pContent
  def content_=(c: Pawn) { pContent = c }

  def printSquare {
    if (content != null)
      content match {
        case p: KingPawn => print(" " + content.player + content.player + " ")
        case p: Pawn     => print(" " + content.player + " ")
      } else print("   ")
  }

  def this(b: Square) {
    this(b.x, b.y)
    pColor = b.color
    pContent = b.content
  }
}

class Board {

  /* ------------------------ Initializations --------------------------------*/
  var grid = Array.tabulate(8, 8)(new Square(_, _))

  var odd = true
  grid.foreach(row => {
    row.map(c =>
      odd match {
        case true  => if (c.y % 2 != 1) c.color = "dark"
        case false => if (c.y % 2 == 1) c.color = "dark"
    })
    odd = !odd
  })

  //Initialization!
  for (i <- 0 until 3)
    for (square: Square <- grid(i) if (square.color == "dark")) {
      square.content = new Pawn(Black)
    }

  for (i <- 5 until 8)
    for (square: Square <- grid(i) if (square.color == "dark")) {
      square.content = new Pawn(White)
    }

  /*grid(0)(0).content = new KingPawn("b")
	grid(1)(1).content = new Pawn("w")
	grid(3)(3).content = new Pawn("w")
	grid(1)(3).content = new KingPawn("w")
	grid(1)(5).content = new Pawn("w")
	
	grid(6)(6).content = new Pawn("w")
	grid(5)(5).content = new Pawn("b")
	grid(3)(3).content = new Pawn("b")
	grid(3)(5).content = new Pawn("b")
	grid(1)(5).content = new Pawn("b")*/

  def printBoard {
    grid.foreach(row => { row.foreach(square => square printSquare); println })
  }
}

/**
  * Companion object including functions to execte moves (side-effect)
  */
object Board {

  def move(grid: Array[Array[Square]], form_x: Int, from_y: Int, to_x: Int, to_y: Int): Boolean = {
    if (grid(form_x)(from_y).content != null) {
      grid(to_x)(to_y).content = grid(form_x)(from_y).content
      grid(form_x)(from_y).content = null
      return true
    }
    false
  }

  def executeMoves(grid: Array[Array[Square]], moves: Array[Move], player: Player) {
    moves.foreach(m => {
      move(grid, m.from_x, m.from_y, m.to_x, m.to_y)
      //m.printMove
      if (m.move_type == "capture") {
        val toCapture = grid((m.from_x + m.to_x) / 2)((m.from_y + m.to_y) / 2)
        //println("mangiata pedina in ("+toCapture.x+","+toCapture.y+") = "+toCapture.content)
        toCapture.content = null
      }
      // If the pawn reaches the first row, becomes a King
      if (m.to_x == 0 && player == White) grid(m.to_x)(m.to_y).content = new KingPawn(White)
      if (m.to_x == 7 && player == Black) grid(m.to_x)(m.to_y).content = new KingPawn(Black)
    })
  }

  /**
	 * Performs implicit conversion from Board to Array[Array[String]]
	 */
  implicit def chessboardToStringMatrix(c: Board) = {
    Array.tabulate(8, 8)((x, y) =>
      c.grid(x)(y).content match {
        case p: KingPawn => p.player.toString.concat(p.player.toString)
        case p: Pawn     => p.player.toString
        case _           => "_"
    })
    //if(c.grid(x)(y).content != null) c.grid(x)(y).content.player else "_")
  }

  /**
	 * Tells if a given move is a valid move for white player
	 */
  def isMoveValid(grid: Array[Array[Square]], m: Move, player: Player, opponent: Player): Boolean = {

    grid(m.from_x)(m.from_y).content match {
      // Case of white king pawn
      case p: KingPawn => {
        // check whether the given move is a correct "move" action, in every direction
        if (abs(m.from_x - m.to_x) == 1 && abs(m.from_y - m.to_y) == 1) {
          if (grid(m.from_x)(m.from_y).content != null && grid(m.to_x)(m.to_y).content == null) {
            return true
          }
        }
        // check whether the given move is instead an "capture" move. Changes the move_type field's value and return true
        if (abs(m.from_x - m.to_x) == 2 && abs(m.from_y - m.to_y) == 2) {
          if (grid(m.to_x)(m.to_y).content == null) {
            val mid_x = (m.from_x + m.to_x) / 2
            val mid_y = (m.from_y + m.to_y) / 2
            if (grid(mid_x)(mid_y).content.player == opponent) {
              // Case Pawn or KingPawn, is the same
              m.move_type = "capture"
              return true
            }
          }
        }
      }
      // Case of simple pawn
      case p: Pawn => {

        // check whether the given move is a correct "move" action
        if (m.from_x - m.to_x == 1 && abs(m.from_y - m.to_y) == 1)
          if (grid(m.from_x)(m.from_y).content != null && grid(m.to_x)(m.to_y).content == null)
            return true

        // check whether the given move is instead an "capture" move. Changes the move_type field's value and return true
        if (m.from_x - m.to_x == 2 && abs(m.from_y - m.to_y) == 2) {
          if (grid(m.to_x)(m.to_y).content == null)
            grid((m.from_x + m.to_x) / 2)((m.from_y + m.to_y) / 2).content match {
              // In case of KingPawn, return false, cause simple Pawn cannot capture KingPawn
              case p: KingPawn =>
              // Case in wich middle pawn is an opponent pawn
              case p: Pawn if (p.player == opponent) => m.move_type = "capture"; return true
              // other cases...
              case _ =>
            }
        }
      }
      // other cases, return false
      case _ =>
    }
    false
  }

  def isSquareWithinBoard(y: Int, x: Int): Boolean = {
    (x > -1 && x < 8 && y > -1 && y < 8)
  }
}

/**
  * Class wich represents a single move, indicating origin and destination
  */
class Move(val from_x: Int, val from_y: Int, val to_x: Int, val to_y: Int, var move_type: String) {
  def printMove {
    if (move_type == "move") print("Muove")
    else if (move_type == "capture") print("Mangia")
    println(" : da (" + from_x + "," + from_y + ") a (" + to_x + "," + to_y + ")")
  }

  override def equals(m: Any) = m match {
    case m: Move => from_x == m.from_x && from_y == m.from_y && to_x == m.to_x && to_y == m.to_y
  }

}
