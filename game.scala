import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable.ListBuffer
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
	
	val intelligence = new Intelligence with DummyEvaluate
	

	var multiple_moves : Array[Array[Move]] = null
	var finished = true

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

	view.setNewGameAction({_ => Game.newGame })
	

	def replayActions() {
		val s = intelligence.minMax(view.getDepth,chessboard.grid,if(view.getHeuristic == "killer heuristic") true else false)
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
		// Force user to choose a "capture" move

		if(isValid){
			
			if(white_must_eat &&  move.move_type != "capture"){
				val moves = intelligence.getPossibleMovesFor(chessboard.grid,"w","b")
				// If the first element of the first move array is a captur move...
				if(moves.length > 0 && moves(0) != null && moves(0).length > 0 && moves(0)(0).move_type == "capture"){
					println("LA MOSSA NON E' VALIDA PERCHE' IL GIOCATORE DEVE MANGIARE!!")
					view.showPopUpMessage("Mossa non valida, il bianco deve mangiare!")
					return false;
				}
			}
			// Force user to choose longest "capture" move 
			if(white_must_eat &&  move.move_type == "capture"){
				val moves = intelligence.getPossibleMovesFor(chessboard.grid,"w","b")
				
				if(moves.length > 0){
					// curr contains all (multiple) moves which first move is 'move'.
					val curr = moves.filter(mv => mv.length > 0 && mv(0) == move)

					if(curr.length > 0){
						var max = moves(0).length
						moves.foreach(m => if(m.length > max) max = m.length)
						// If the length of curr moves is less then the maximum, communicate error to user and stop
						if(curr(0).length < max){
							println("LA MOSSA NON E' VALIDA PERCHE' IL GIOCATORE DEVE SCEGLIERE LA MOSSA DI CATTURA PIU LUNGA")
							view.showPopUpMessage("Mossa non valida, il bianco deve scegliere la mossa di cattura più lunga!")
							return false
						}
						
						// update multiple moves to see if white user has multiple moves to do 
						if (multiple_moves == null) multiple_moves = curr.filter(m => m.length == max)
						multiple_moves = multiple_moves.map(move_array =>
							if(move_array!=null && move_array(0) == move) move_array.drop(1) else null
						)

						// Check if has finished 
						finished = true
						multiple_moves.foreach(m => if(m!=null && m.length > 0) finished = false)
						println("finished? "+finished)

					}else{
						println("LA MOSSA NON E' VALIDA PERCHE' IL GIOCATORE DEVE SCEGLIERE LA MOSSA DI CATTURA PIU LUNGA")
						view.showPopUpMessage("Mossa non valida, il bianco deve scegliere la mossa di cattura più lunga!")
						return false;
					}
				}
			}
			
			Chessboard.executeMoves(chessboard.grid,Array(move),"w")
			view.updateChessboard(chessboard)
			if (finished) {
				view.setStatus("b","Black moves")
				multiple_moves = null
				// delegate to an actor opponent's reply
				view.showLoadingPopUp
				actor {
					reactWithin(300){
						case TIMEOUT => replayActions;view.hideLoadingPopUp;view.setStatus("w","White moves")
					}
				}
			}else{
				multiple_moves.foreach(a => {if(a!=null) a.foreach(m => m.printMove);println})
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

	def newGame() {instance.chessboard = new Chessboard;instance.view.updateChessboard(instance.chessboard) }

}

object Main extends App{
	override def main(a:Array[String]){
		//Game.white_must_eat = false
		Game.getInstance
	}
}

