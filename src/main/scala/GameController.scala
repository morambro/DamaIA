import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable.ListBuffer
import java.util._;
import java.text.MessageFormat;
import Player._
/**
  * Logic class which coordinates the different parts of the game. It offers also methods callable by the View.
  *
  */
class Game private (val white_must_capture: Boolean) {
  var currentLocale = Locale.getDefault();
  var messages = ResourceBundle.getBundle("MessagesBundle", currentLocale);
  var pView = new BoardView
  var formatter = new MessageFormat("");
  formatter.setLocale(currentLocale);

  def view = pView
  def view_=(view: BoardView) { pView = view }

  var pBoard = new Board

  view.updateBoard(board)

  def board = pBoard
  def board_=(board: Board) { pBoard = board }

  val engine = new Engine

  var multiple_moves: Array[Array[Move]] = null
  var finished                           = true

  view.setOperationForBoard(
    (x, y, i, j) => updateBoard(x, y, i, j)
  )

  /**
	 * set Operation to be called by View to make Computer play
	 */
  view.setReplyAction({ _ =>
    {
      replayActions
    }
  })

  view.setNewGameAction({ _ =>
    Game.newGame
  })

  def replayActions() {
    val heur = if (view.getHeuristic == "killer heuristic") true else false
    val s    = engine.minimax(view.getDepth, board.grid, heur, view.getEval)

    if (s._2 != null) {
      {
        var stuff:Array[Object]=new Array[Object](1); stuff(0)= new Integer (s._1)
        formatter.applyPattern(messages.getString("minimaxValue"));
        println(formatter.format(stuff))
      }
      s._2.foreach(m => m.printMove)
      Board.executeMoves(board.grid, s._2, Black)
      view.updateBoard(board)
      board.printBoard
    } else {
      println(messages.getString("noLegalMoves"));
      view.showPopUpMessage(messages.getString("gameOverWhiteWins"));
    }

    // If white doesn't have other legal moves, communicates that black wins!
    if (engine.getLegalMovesFor(board.grid, White, Black).length == 0)
      view.showPopUpMessage(messages.getString("gameOverBlackWins"));
  }

  /**
	 * Method callable by the view after human interaction (white player move!!)
	 */
  def updateBoard(x: Int, y: Int, to_x: Int, to_y: Int): Boolean = {
    val move    = new Move(x, y, to_x, to_y, "move")
    val isLegal = Board.isMoveLegal(board.grid, move, White, Black)
    // Force user to choose a "capture" move

    if (isLegal) {

      if (white_must_capture && move.move_type != "capture") {
        val moves = engine.getLegalMovesFor(board.grid, White, Black)
        // If the first element of the first move array is a captur move...
        if (moves.length > 0 && moves(0) != null && moves(0).length > 0 && moves(0)(0).move_type == "capture") {
          println(messages.getString("illegalMove") + " " + messages.getString("captureIsMandatory"));
          view.showPopUpMessage(messages.getString("illegalMove") + " " + messages.getString("captureIsMandatory"));
          return false;
        }
      }
      // Force user to choose longest "capture" move
      if (white_must_capture && move.move_type == "capture") {
        val moves = engine.getLegalMovesFor(board.grid, White, Black)

        if (moves.length > 0) {
          // curr contains all (multiple) moves which first move is 'move'.
          val curr = moves.filter(mv => mv.length > 0 && mv(0) == move)

          if (curr.length > 0) {
            var max = moves(0).length
            moves.foreach(m => if (m.length > max) max = m.length)
            // If the length of curr moves is less then the maximum, communicate error to user and stop
            if (curr(0).length < max) {
              println(messages.getString("illegalMove") + " " + messages.getString("maxCaptureIsMandatory"));
              view.showPopUpMessage(messages.getString("illegalMove") + " " + messages.getString("maxCaptureIsMandatory"));
              return false
            }

            // update multiple moves to see if white user has multiple moves to do
            if (multiple_moves == null) multiple_moves = curr.filter(m => m.length == max)
            multiple_moves = multiple_moves.map(move_array =>
              if (move_array != null && move_array(0) == move) move_array.drop(1) else null)

            // Check if has finished
            finished = true
            multiple_moves.foreach(m => if (m != null && m.length > 0) finished = false)
            println("finished? " + finished)

          } else {
            println(messages.getString("illegalMove") + " " + messages.getString("maxCaptureIsMandatory"));
            view.showPopUpMessage(messages.getString("illegalMove") + " " + messages.getString("maxCaptureIsMandatory"));
            return false;
          }
        }
      }

      Board.executeMoves(board.grid, Array(move), White)
      view.updateBoard(board)
      if (finished) {
        view.setStatus(Black.toString, messages.getString("blackMoves"))
        multiple_moves = null
        // delegate to an actor opponent's reply
        view.showLoadingPopUp
        actor {
          reactWithin(50) {
            case TIMEOUT =>
              replayActions; view.hideLoadingPopUp; view.setStatus(White.toString, messages.getString("whiteMoves"))
          }
        }
      } else {
        multiple_moves.foreach(a => { if (a != null) a.foreach(m => m.printMove); println })
      }
    }
    isLegal
  }
}

/**
  * Companion Object used to implement Singleton design pattern
  */
object Game {

  var white_must_capture = true;

  var instance: Game = null

  def getInstance() = {
    if (instance == null) instance = new Game(white_must_capture)
    instance
  }

  def newGame() {
    instance.board = new Board; instance.view.updateBoard(instance.board)
  }

}

object Main extends App {
  override def main(a: Array[String]) {
    var currentLocale = Locale.getDefault();
    var messages = ResourceBundle.getBundle("MessagesBundle", currentLocale);
    println(messages.getString("testMsg"));
    //Game.white_must_capture = false
    Game.getInstance
  }
}
