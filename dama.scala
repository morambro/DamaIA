class Pawn(val player:String){
	
	override def equals(o : Any) = o match{case p:Pawn => player==p.player}
	
	def this(p : Pawn){
		this(p.player)
	}
}

class KingPawn(player : String) extends Pawn(player)

/**
 * logic box of the chessboard 
 */
class Box(val x:Int,val y:Int){
	
	var pColor = "dark"
	var pContent : Pawn = null
	
	def color = pColor
	def color_= (c : String) {pColor = c} 
	
	def content = pContent
	def content_= (c : Pawn) {pContent = c}
	
	def printBox{
		if(content != null)
			content match{
				case p:KingPawn => print(" "+content.player+content.player+" ")
				case p:Pawn => print(" "+content.player+" ") 
			}
		else print("   ")
	}
	
	def this(b : Box){
		this(b.x,b.y)
		pColor = b.color
		pContent = b.content
	}
}

import scala.math._

class Chessboard{
	
	/* ------------------------ Initializations --------------------------------*/
	var grid = Array.tabulate(8,8)(new Box(_,_))
	
	var odd = true
	grid.foreach( row => {
			row.map(c => 
				odd match {
					case true => if(c.y%2 !=1)c.color = "light"
					case false => if(c.y%2 ==1)c.color = "light"
				}
			)
			odd = !odd
		}
	)
	
	for(i <- 0 until 3)
		for(box:Box <- grid(i) if(box.color == "light")){
			box.content = new KingPawn("b")
		}
	
	for(i <- 5 until 8)
		for(box:Box <- grid(i) if(box.color == "light")){
			box.content = new Pawn("w")
		}
		
	def printBoard{
		grid.foreach(row => {row.foreach( box => box printBox) ; println})
	}
}

/**
 * Companion object including functions to execte moves (side-effect)
 */
object Chessboard{

	def move(grid:Array[Array[Box]],form_x:Int,from_y:Int,to_x:Int,to_y:Int,player:String):Boolean = {
		if(grid(form_x)(from_y).content != null){
			grid(to_x)(to_y).content = grid(form_x)(from_y).content
			grid(form_x)(from_y).content = null
			return true
		}
		false
	}
	
	def executeMoves(grid:Array[Array[Box]],moves : Array[Move],player : String){
		moves.foreach(m => {
			move(grid,m.from_x,m.from_y,m.to_x,m.to_y,player)
			//m.printMove
			if(m.move_type == "eat"){
				val toEat = grid((m.from_x+m.to_x)/2)((m.from_y+m.to_y)/2)
				//println("mangiata pedina in ("+toEat.x+","+toEat.y+") = "+toEat.content)
				toEat.content = null
			}
			// If the pawn reaches the first row, becomes a King
			if(m.to_x == 0 && player == "w") grid(m.to_x)(m.to_y).content = new KingPawn("w")
			if(m.to_x == 7 && player == "b") grid(m.to_x)(m.to_y).content = new KingPawn("b")
		})
	}
	
	/**
	 * Performs implicit conversion from Chessboard to Array[Array[String]] 
	 */
	implicit def chessboardToStringMatrix(c:Chessboard) = {
		Array.tabulate(8,8)(
			(x,y) => c.grid(x)(y).content match {
				case p : KingPawn => p.player.concat(p.player)
				case p : Pawn => p.player
				case _ => "_"
			})
			//if(c.grid(x)(y).content != null) c.grid(x)(y).content.player else "_")
	}
	
	/**
	 * Tells if a move is valid 
	 */
	def isMoveValid(grid:Array[Array[Box]],m:Move):Boolean = {
		println("("+m.from_x+","+m.from_y+")  ("+m.to_x+","+m.to_y+")")
		
		grid(m.from_x)(m.from_y).content match {
			// Case of white king pawn
			case p : KingPawn => {
				// check whether the given move is a correct "move" action, in every direction
				if(abs(m.from_x - m.to_x) == 1 && abs(m.from_y - m.to_y) == 1) {
					if(grid(m.from_x)(m.from_y).content!=null && grid(m.to_x)(m.to_y).content==null){
						return true
					}
				}
				// check whether the given move is instead an "eat" move. Changes the move_type field's value and return true 
				if(abs(m.from_x - m.to_x) == 2 && abs(m.from_y - m.to_y) == 2) {
					if(grid(m.to_x)(m.to_y).content==null) 
						grid((m.from_x+m.to_x)/2)((m.from_y+m.to_y)/2).content.player match {
							// Case Pawn or KingPawn, is the same
							case "b" => { 
								m.move_type = "eat"
								return true
							}
						}
				}
			}
			// Case of simple pawn
			case p : Pawn => {
				// check whether the given move is a correct "move" action
				if(m.from_x - m.to_x == 1 && abs(m.from_y - m.to_y) == 1) {
					if(grid(m.from_x)(m.from_y).content!=null && grid(m.to_x)(m.to_y).content==null){
						return true
					}
				}
				// check whether the given move is instead an "eat" move. Changes the move_type field's value and return true 
				if(m.from_x - m.to_x == 2 && abs(m.from_y - m.to_y) == 2) {
					if(grid(m.to_x)(m.to_y).content==null)
						grid((m.from_x+m.to_x)/2)((m.from_y+m.to_y)/2).content match {
							// In case of KingPawn, return false, cause simple Pawn cannot eat KingPawn
							case p : KingPawn => return false
							case p : Pawn => m.move_type = "eat";return true
						}
				}
			}
			case _ => 
		}
		false
	}
}


/**
 * Class wich represents a single move, indicating origin and destination
 */
class Move(val from_x:Int,val from_y:Int,val to_x:Int,val to_y:Int,var move_type : String){
	def printMove {
		if(move_type == "move")print("Muove")
		else if (move_type == "eat")print("Mangia")
		println (" : da ("+from_x+","+from_y+") a ("+to_x+","+to_y+")")
	}
}

import scala.collection.mutable.ListBuffer

/** 
 * Class which represents the intelligent engine of the game 
 */
class Intelligence{

	val MAX = "b"
	val MIN = "w"


	private def getPawnMove(grid:Array[Array[Box]], from_x:Int, from_y:Int, x:Int, y:Int, inc:Int => Int,opponent:String) : Move = {
		/* look at what there is in the near box */
		grid(x)(y).content match{
			case null => return new Move(from_x,from_y,x,y,"move")
			case p : KingPawn => 
			// Case in wich there is a simple opponent pawn
			case value : Pawn if(value.player == opponent)  => {
				val res = canEat(grid,from_x,from_y,(a:Int,b:Int) => (inc(inc(a)),b+2))
				if(res != null) return new Move(from_x,from_y,res._1,res._2,"eat") 
			}
			case _ => 
		}
		null
	}
	
	private def getKingPawnMove(grid:Array[Array[Box]], from_x:Int, from_y:Int, x:Int, y:Int, inc:Int => Int,opponent:String) : Move = {
		/* look at what there is in the near box */
		grid(x)(y).content match{
			case null => return new Move(from_x,from_y,x,y,"move")
			// Every pawn is ok!
			case value : Pawn if(value.player == opponent)  => {
				val res = canEat(grid,from_x,from_y,(a:Int,b:Int) => (inc(inc(a)),b+2))
				if(res != null) return new Move(from_x,from_y,res._1,res._2,"eat") 
			}
			case _ => 
		}
		null
	}

	/**
	 * Given a configuration (grid), a player and his opponent, returns all the possible moves for the current player
	 *
	 * @param grid : configuration
	 * @param player : current player
	 * @param opponent : the opponent
	 *
	 * return all possible moves from the given configuration
	 */
	def getPossibleMovesFor(grid:Array[Array[Box]],player:String,opponent:String):Array[Move] = {
		var moves = ListBuffer[Move]()
		val inc = if(player == MAX) (x:Int) => x+1 else (x:Int) => x-1
		val dec = if(player == MAX) (x:Int) => x-1 else (x:Int) => x+1
		
		/* run over all the boxes, filtering them by content and valid increment */
		grid.foreach( r => r.filter( b => b.content != null && b.content.player == player && inc(b.x) < 8 && inc(b.x) > -1).foreach( b =>
			b.content match { 
				// Check through all possible directions
				case p : KingPawn =>  {
					if(b.x + 1 < 8){
						if(b.y + 1 < 8){
							val move = getKingPawnMove(grid,b.x,b.y,b.x+1,b.y+1,inc,opponent)
							if(move != null) moves += move
						}
						if(b.y - 1 > -1){
							val move = getKingPawnMove(grid,b.x,b.y,b.x+1,b.y-1,inc,opponent)
							if(move != null) moves += move
						}
					}
					if(b.x - 1 > 0){
						if(b.y + 1 < 8){
							val move = getKingPawnMove(grid,b.x,b.y,b.x-1,b.y+1,dec,opponent)
							if(move != null) moves += move
						}
						if(b.y - 1 > -1){
							val move = getKingPawnMove(grid,b.x,b.y,b.x-1,b.y-1,dec,opponent)
							if(move != null) moves += move
						}
					}
				}
				// Check only in one direction
				case p : Pawn => {
					if(b.y + 1 < 8) {
						val move = getPawnMove(grid,b.x,b.y,inc(b.x),b.y+1,inc,opponent)
						if(move != null) moves += move
					}
					if(b.y - 1 > -1) {
						val move = getPawnMove(grid,b.x,b.y,inc(b.x),b.y-1,inc,opponent)
						if(move != null) moves += move
					}
				} 
		}))
		/* If player can eat, is forced to eat! */
		val eat_moves = moves.filter(move => move.move_type == "eat")
		if(eat_moves.length > 0) return eat_moves.toArray
		moves.toArray
	}
	
	/**
	 * Implements minmax algorithm. It decides the best move supposing the opponent plays in optimal mode. 
	 *
	 * @param dept : maximum depth for the minmax tree of recoursive calls
	 * @param grid : represents the current situation of the chessboard
	 * @param player : the player for wich to decide the best move at the current tree level
	 * 
	 * return : a couple of elemnents, composed by the value of the current state of the chessboard and the move selected
	 */	
	def minMax(depth : Int, grid : Array[Array[Box]]) : (Int,Move) = {
		return maxMove(depth,grid,Int.MaxValue,Int.MinValue)
	}
	
	def maxMove(depth : Int, game : Array[Array[Box]], alpha : Int, beta : Int) : (Int,Move) = {
		if(depth == 0) {
			//println("valutazione per player (depth = 0) +"+player+" : "+evaluate(player,grid))
			return (evaluate(MAX,game),null)
		}
		val moves = getPossibleMovesFor(game,MAX,MIN)
		if(moves == null || moves.length == 0) return (evaluate(MAX,game),null)
		var best_move : (Int,Move) = null
		var new_alpha = alpha
		moves.foreach(move => {
			var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(game(x)(y)))
			Chessboard.executeMoves(new_grid,Array(move),MAX)
			// tocca a Min!
			val min_move = minMove(depth-1,new_grid,alpha,beta)
			if(best_move == null || best_move._1 < min_move._1){
				best_move = (min_move._1,move)
				new_alpha = best_move._1
			}
			if(beta > alpha && best_move!=null) return best_move
		})
		return best_move
	}

	def minMove(depth : Int,game : Array[Array[Box]], alpha : Int, beta : Int) : (Int,Move) = {
		if(depth == 0) {
			//println("valutazione per player (depth = 0) +"+player+" : "+evaluate(player,grid))
			return (evaluate(MAX,game),null)
		}
		val moves = getPossibleMovesFor(game,MAX,MIN)
		if(moves == null || moves.length == 0) return (evaluate(MAX,game),null)
		var best_move : (Int,Move) = null
		var new_beta = beta
		moves.foreach(move => {
			// Costruisco una nuova scacchiera applicando la mossa corrente
			var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(game(x)(y)))
			Chessboard.executeMoves(new_grid,Array(move),MIN)
			// tocca a Min!
			val max_move = maxMove(depth-1,new_grid,alpha,beta)
			if(best_move == null || best_move._1 > max_move._1){
				best_move = (max_move._1,move)
				new_beta = best_move._1
			}
			if(beta < alpha && best_move!=null) return best_move
		})
		return best_move
	}
	
	/**
	 * tells if there could be a move of type "eat" from the given position and given direction
	 */
	def canEat( grid:Array[Array[Box]], x:Int, y:Int, direction:(Int,Int)=>(Int,Int)) : (Int,Int) = {
		val (new_x,new_y) = direction(x,y)
		//	println("("+x+","+y+")  ("+new_x+","+new_y+")")
		if(new_x < 8 && new_y < 8 && new_y >= 0 && new_x > -1 && grid(new_x)(new_y).content == null){
			// if there's an empty cell after the opponent cell, through the given direction, say yes and return the new position 
			grid(x)(y).content match {
				case p : KingPawn => return (new_x,new_y)
				case p : Pawn => grid((x+new_x)/2)((y+new_y)/2).content match {
						case _ : KingPawn => 
						case _ : Pawn => return (new_x,new_y)
						case _ =>
					}
				case _ => // Does Nothing
			}
		}
		null
	}
	
	/**
	 * Very primitive heuristic function. Evaluates the goodness of the 
	 * situation by giving the difference between the number of pieces of 
	 * the white player and the number of pieces of the opponent
	 */
	def evaluate(player:String,grid:Array[Array[Box]]) = {
		var num_player = 0
		var num_opponent = 0
		grid.foreach(row => row.map(c => {
			if(c.content != null && c.content.player == MAX) num_player+=1 
			else if(c.content != null) num_opponent += 1
		}))
		
		num_player - num_opponent
	}		
}
