import scala.actors._
import scala.actors.Actor._
/**
 * Logic class which coordinates the different parts of the game. It offers also methods callable by the View.
 *
 */
class Game private(val white_must_eat:Boolean){
	var pView = new ChessboardView
	
	def view = pView
	def view_= (view:ChessboardView) {pView = view}
	
	var pChessboard = new Chessboard

	view.updateChessboard(chessboard)

	def chessboard = pChessboard
	def chessboard_= (chessboard:Chessboard) {pChessboard = chessboard}
	
	val intelligence = new Intelligence
	
	view.setOperationForChessboard(
		(x,y,i,j) => updateChessboard(x,y,i,j)
	)
	
	/**
	 * set Operation to be called by View to make Computer play 
	 */
	view.setReplyAction({
		_ => {
			replayActions
		}
	})
	

	def replayActions() {
		val s = intelligence.minMax(1,chessboard.grid)
		print("\n"+s)
		print("valore minmax per 'b' = "+s._1+" mossa : "); 
		if(s._2 != null) {
			Chessboard.executeMoves(chessboard.grid,s._2,"b")
			view.updateChessboard(chessboard)
			chessboard.printBoard
		} else {
			println("\nNessuna mossa possibile!")
			view.showPopUpMessage("Partita finita, il bianco vince!")
		}

		// If white doesn't have other possible moves, communicates that black wins!
		if(intelligence.getPossibleMovesFor(chessboard.grid,"w","b").length == 0) 
			view.showPopUpMessage("Partita finita, il nero vince!")
	}
	
	/**
	 * Method callable by the view after human interaction (white player move!!)
	 */
	def updateChessboard(x:Int,y:Int,to_x:Int,to_y:Int): Boolean = {
		val move = new Move(x,y,to_x,to_y,"move")
		val isValid = Chessboard.isMoveValid(chessboard.grid,move,"w","b")
		println(white_must_eat)
		if(white_must_eat &&  move.move_type != "capture"){
			val moves = intelligence.getPossibleMovesFor(chessboard.grid,"w","b")
			// If the first element of the first move array is a captur move...
			if(moves.length > 0 && moves(0)(0).move_type == "capture"){
				println("LA MOSSA NON E' VALIDA PERCHE' IL GIOCATORE DEVE MANGIARE!!")
				view.showPopUpMessage("Mossa non valida, il bianco deve mangiare!")
				return false;
			}
		}
		if(isValid){
			Chessboard.executeMoves(chessboard.grid,Array(move),"w")
			view.updateChessboard(chessboard)
			// delegate to an actor opponent's reply
			view.showLoadingPopUp
			actor {
				reactWithin(300){
					case TIMEOUT => replayActions;view.hideLoadingPopUp
				}
			}
		}
		isValid
	}
	
}

/**
 * Companion Object used to implement Singleton design pattern
 */
object Game{

	var white_must_eat = true;

	var instance : Game = null

	def getInstance() = {
		if(instance == null) instance = new Game(white_must_eat)	
		instance
	}
}

object Main extends App{
	override def main(a:Array[String]){
		Game.white_must_eat = false
		Game.getInstance
	}
}

