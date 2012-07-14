/**
 * Logic class which coordinates the different parts of the game. It offers also methods callable by the View.
 *
 */
class Game private(){
	
	var pView = new ChessboardView
	
	def view = pView
	def view_= (view:ChessboardView) {pView = view}
	
	var pChessboard = new Chessboard 
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
			val s = intelligence.minMax(10,chessboard.grid)
			print("\n"+s)
			print("valore minmax per 'b' = "+s._1+" mossa : "); 
			if(s._2 != null) {
				s._2.printMove
				Chessboard.executeMoves(chessboard.grid,Array(s._2),"b")
				view.updateChessboard(chessboard)
				chessboard.printBoard
			} else {
				//TODO : Comunicare la fine della partita!
				println("\nNessuna mossa possibile!")
			}
		}
	})
	
	
	/**
	 * Method callable by the view after human interaction (white player move!!)
	 */
	def updateChessboard(x:Int,y:Int,to_x:Int,to_y:Int): Boolean = {
		val move = new Move(x,y,to_x,to_y,"move")
		val isValid = Chessboard.isMoveValid(chessboard.grid,move,"w","b")
		if(move.move_type != "eat"){
			val moves = intelligence.getPossibleMovesFor(chessboard.grid,"w","b")
			if(moves.length > 0 && moves(0).move_type == "eat"){
				println("LA MOSSA NON E' VALIDA PERCHE' IL GIOCATORE DEVE MANGIARE!!")
				return false;
			}
		}
		if(isValid){
			Chessboard.executeMoves(chessboard.grid,Array(move),"w")
			view.updateChessboard(chessboard)
		}
		isValid
	}
	
}

/**
 * Companion Object used to implement Singleton design pattern
 */
object Game{
	val instance = new Game()
	def getInstance() = instance	
}


object Main extends App{
	val game = Game.getInstance
}

