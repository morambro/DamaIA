class Pawn(val player:String){
	
	override def equals(o : Any) = o match{case p:Pawn => player==p.player}
	
	def this(p : Pawn) {this(p.player)}
}

class KingPawn(player : String) extends Pawn(player)

/**
 * logic box of the chessboard 
 */
class Box(val x:Int,val y:Int){
	
	var pColor = "light"
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
					case true => if(c.y%2 !=1)c.color = "dark"
					case false => if(c.y%2 ==1)c.color = "dark"
				}
			)
			odd = !odd
		}
	)
	
	//Initialization!
	/*for(i <- 0 until 3)
		for(box:Box <- grid(i) if(box.color == "dark")){
			box.content = new Pawn("b")
		}
	
	for(i <- 5 until 8)
		for(box:Box <- grid(i) if(box.color == "dark")){
			box.content = new Pawn("w")
		}*/
	 
	grid(0)(0).content = new KingPawn("b")
	grid(1)(1).content = new Pawn("w")
	grid(3)(3).content = new Pawn("w")
	grid(1)(3).content = new KingPawn("w")
	grid(1)(5).content = new Pawn("w")


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
			if(m.move_type == "capture"){
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
	 * Tells if a given move is a valid move for white player 
	 */
	def isMoveValid(grid:Array[Array[Box]],m:Move,player : String,opponent : String):Boolean = {

		grid(m.from_x)(m.from_y).content match {
			// Case of white king pawn
			case p : KingPawn => {
				// check whether the given move is a correct "move" action, in every direction
				if(abs(m.from_x - m.to_x) == 1 && abs(m.from_y - m.to_y) == 1) {
					if(grid(m.from_x)(m.from_y).content!=null && grid(m.to_x)(m.to_y).content==null){
						return true
					}
				}
				// check whether the given move is instead an "capture" move. Changes the move_type field's value and return true 
				if(abs(m.from_x - m.to_x) == 2 && abs(m.from_y - m.to_y) == 2) {
					if(grid(m.to_x)(m.to_y).content==null) {
						val mid_x = (m.from_x+m.to_x)/2 
						val mid_y = (m.from_y+m.to_y)/2
						if(grid(mid_x)(mid_y).content.player == opponent) {
								// Case Pawn or KingPawn, is the same
							m.move_type = "capture"
							return true
						}
					}
				}
			}
			// Case of simple pawn
			case p : Pawn => {

				// check whether the given move is a correct "move" action
				if(m.from_x - m.to_x == 1 && abs(m.from_y - m.to_y) == 1) 
					if(grid(m.from_x)(m.from_y).content!=null && grid(m.to_x)(m.to_y).content==null) return true

				// check whether the given move is instead an "capture" move. Changes the move_type field's value and return true 
				if(m.from_x - m.to_x == 2 && abs(m.from_y - m.to_y) == 2) {
					if(grid(m.to_x)(m.to_y).content==null)
						grid((m.from_x+m.to_x)/2)((m.from_y+m.to_y)/2).content match {
							// In case of KingPawn, return false, cause simple Pawn cannot eat KingPawn
							case p : KingPawn => 
							// Case in wich middle pawn is an opponent pawn
							case p : Pawn if(p.player == opponent) => m.move_type = "capture";return true
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
}

/**
 * Class wich represents a single move, indicating origin and destination
 */
class Move(val from_x:Int,val from_y:Int,val to_x:Int,val to_y:Int,var move_type : String){
	def printMove {
		if(move_type == "move")print("Muove")
		else if (move_type == "capture")print("Mangia")
		println (" : da ("+from_x+","+from_y+") a ("+to_x+","+to_y+")")
	}

	override def equals(m : Any) = m match {
		case m : Move => from_x == m.from_x && from_y == m.from_y && to_x == m.to_x && to_y == m.to_y
	}
	
}

import scala.collection.mutable.ListBuffer

/** 
 * Class which represents the intelligent engine of the game 
 */
class Intelligence{

	val MAX = "b"
	val MIN = "w"


	/**
	 * tells if there could be a move of type "capture" from the given position and given direction
	 */
	def canEat( grid:Array[Array[Box]], x:Int, y:Int, direction:(Int,Int)=>(Int,Int)) : (Int,Int) = {
		val (new_x,new_y) = direction(x,y)
		if(new_x < 8 && new_y < 8 && new_y > -1 && new_x > -1 && grid(new_x)(new_y).content == null){
			// if there's an empty cell after the opponent cell, through the given direction, say yes and return the new position 
			grid(x)(y).content match {
				case p : KingPawn => grid((x+new_x)/2)((y+new_y)/2).content match {
					case null =>
					case s : Pawn if(p.player != s.player) => return (new_x,new_y)
					case _ =>
				}	
				case p : Pawn => grid((x+new_x)/2)((y+new_y)/2).content match {
						case _ : KingPawn => 
						case s : Pawn if(p.player != s.player)=> return (new_x,new_y)
						case _ => 
				}
				case _ => // Does Nothing
			}
		}
		null
	}

	/**
	 * Inspects the chessboard searching for a multiple capture for the pawn in (f_x,f_y)
	 *
	 * @param grid : the chessboard
	 * @param f_x,f_y : current position
	 * @param inc : increment function giving the x-direction (from top to bottom or from bottom to top)
	 * @param current_move : Move list to wich append new moves
	 * @param total_moves : list of lists of moves containing all moves found (initially empty)
	 */
	def checkForMultipleCapture(grid:Array[Array[Box]],f_x:Int,f_y:Int,inc:Int => Int,current_move : ListBuffer[Move],total_moves:ListBuffer[ListBuffer[Move]]){
		grid(f_x)(f_y).content match{
			case null =>
			case p:KingPawn => {
				val res_left = canEat(grid,f_x,f_y,(a:Int,b:Int) => (a+2,b-2))
				val res_right = canEat(grid,f_x,f_y,(a:Int,b:Int) => (a+2,b+2))
				val res_up_right = canEat(grid,f_x,f_y,(a:Int,b:Int) => (a-2,b+2))
				val res_up_left = canEat(grid,f_x,f_y,(a:Int,b:Int) => (a-2,b-2))
				
				// If i found a possible capture "on the left" add the new move to current_move
				if(res_left != null) {
					current_move += new Move(f_x,f_y,res_left._1,res_left._2,"capture")
					var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(grid(x)(y)))
					Chessboard.executeMoves(new_grid,current_move.toArray,MAX)
					// recoursive call
					checkForMultipleCapture(new_grid,res_left._1,res_left._2,(_+1),current_move,total_moves)
				}
				
				if(res_right != null) {
					// Create a copy of the current_move list only if there was another move on the left (separates the moves)
					var mult_moves_right : ListBuffer[Move] = null
					if(res_left == null) mult_moves_right = current_move
					else {
						// create a new ListBuffer containing common moves
						mult_moves_right = current_move.clone;mult_moves_right.trimEnd(1)
						// Add the new ListBuffer to total_moves
						total_moves += mult_moves_right
					}
					
					mult_moves_right += new Move(f_x,f_y,res_right._1,res_right._2,"capture")
					var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(grid(x)(y)))
					Chessboard.executeMoves(new_grid,mult_moves_right.toArray,MAX)
					// recoursive call
					checkForMultipleCapture(new_grid,res_right._1,res_right._2,(_+1),mult_moves_right,total_moves)
				
				}

				if(res_up_left != null){
					var mult_moves_right : ListBuffer[Move] = null
					if(res_right != null || res_left != null){
						// create a new ListBuffer containing common moves
						mult_moves_right = current_move.clone;mult_moves_right.trimEnd(1)
						// Add the new ListBuffer to total_moves
						total_moves += mult_moves_right
					}else mult_moves_right = current_move

					mult_moves_right += new Move(f_x,f_y,res_up_left._1,res_up_left._2,"capture")
					var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(grid(x)(y)))
					Chessboard.executeMoves(new_grid,mult_moves_right.toArray,MAX)
					// recoursive call
					checkForMultipleCapture(new_grid,res_up_left._1,res_up_left._2,(_-1),mult_moves_right,total_moves)
				}

				if(res_up_right != null){
					var mult_moves_right : ListBuffer[Move] = null
					if(res_right != null || res_left != null || res_up_left != null){
						// create a new ListBuffer containing common moves
						mult_moves_right = current_move.clone;mult_moves_right.trimEnd(1)
						// Add the new ListBuffer to total_moves
						total_moves += mult_moves_right
					}else mult_moves_right = current_move

					mult_moves_right += new Move(f_x,f_y,res_up_right._1,res_up_right._2,"capture")
					var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(grid(x)(y)))
					Chessboard.executeMoves(new_grid,mult_moves_right.toArray,MAX)
					// recoursive call
					checkForMultipleCapture(new_grid,res_up_right._1,res_up_right._2,(_-1),mult_moves_right,total_moves)
				}
			}
			case p:Pawn => {
				val res_left = canEat(grid,f_x,f_y,(a:Int,b:Int) => (inc(inc(a)),b-2))
				val res_right = canEat(grid,f_x,f_y,(a:Int,b:Int) => (inc(inc(a)),b+2))
				
				// If i found a possible capture "on the left" add the new move to current_move
				if(res_left != null) {
					current_move += new Move(f_x,f_y,res_left._1,res_left._2,"capture")
					var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(grid(x)(y)))
					Chessboard.executeMoves(new_grid,current_move.toArray,MAX)
					// recoursive call
					checkForMultipleCapture(new_grid,res_left._1,res_left._2,inc,current_move,total_moves)
				}
				
				if(res_right != null) {
					// Create a copy of the current_move list only if there was another move on the left (separates the moves)
					var mult_moves_right : ListBuffer[Move] = null
					if(res_left == null) mult_moves_right = current_move
					else {
						// create a new ListBuffer containing common moves
						mult_moves_right = current_move.clone;mult_moves_right.trimEnd(1)
						// Add the new ListBuffer to total_moves
						total_moves += mult_moves_right
					}
					
					mult_moves_right += new Move(f_x,f_y,res_right._1,res_right._2,"capture")
					var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(grid(x)(y)))
					Chessboard.executeMoves(new_grid,mult_moves_right.toArray,MAX)
					// recoursive call
					checkForMultipleCapture(new_grid,res_right._1,res_right._2,inc,mult_moves_right,total_moves)
				}
			}
		}
	}


	/** 
	 * Creates the move for a simple pawngiven from-coordinates and to-coordiantes. It checks if there is a possible "capture" move.
	 *
	 * @param grid : current configuration
	 * @param form_x,from_y : from-coordinates
	 * @param x,y : to-coordiantes
	 * @param inc : function wich increments properly the x coordinate
	 * @param inc_y : function used to increment the y coordinate when searching for an "capture" move
	 */
	private def getPawnMove(grid:Array[Array[Box]], from_x:Int, from_y:Int, x:Int, y:Int, inc:Int => Int,inc_y : Int => Int,opponent:String) : ListBuffer[ListBuffer[Move]] = {
		/* look at what there is in the near box */
		if(x < 0 || x > 7 || y < 0 || y > 7) return null
		grid(x)(y).content match{
			case null => val b = new ListBuffer[ListBuffer[Move]]();b += ListBuffer(new Move(from_x,from_y,x,y,"move"));return b
			case p : KingPawn => 
			// Case in wich there is a simple opponent pawn
			case p : Pawn => {
				var res = canEat(grid,from_x,from_y,(a:Int,b:Int) => (inc(inc(a)),inc_y(b)))
				var t = new ListBuffer[ListBuffer[Move]]()
				if(res != null) {
					t += new ListBuffer[Move](); t(0) += new Move(from_x,from_y,res._1,res._2,"capture")
					var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(grid(x)(y)))
					Chessboard.executeMoves(new_grid,t(0).toArray,MAX)
					checkForMultipleCapture(new_grid,res._1,res._2,inc,t(0),t)
				}
				return t
			}
			case _ => 
		}
		null
	}
	
	/** 
	 * Creates the move for a king pawn given from-coordinates and to-coordiantes. It checks if there is a possible "capture" move.
	 *
	 * @param grid : current configuration
	 * @param form_x,from_y : from-coordinates
	 * @param x,y : to-coordiantes
	 * @param inc : function wich increments properly the x coordinate
	 * @param inc_y : function used to increment the y coordinate when searching for an "capture" move
	 */
	private def getKingPawnMove(grid:Array[Array[Box]], from_x:Int, from_y:Int, x:Int, y:Int, inc: Int => Int,inc_y : Int => Int,opponent:String) : ListBuffer[ListBuffer[Move]] = {
		/* look at what there is in the near box */
		if(x < 0 || x > 7 || y < 0 || y > 7) return null
		grid(x)(y).content match{
			case null => val b = new ListBuffer[ListBuffer[Move]]();b += ListBuffer(new Move(from_x,from_y,x,y,"move"));return b
			// Every pawn is ok!
			case p : Pawn => {
				val res = canEat(grid,from_x,from_y,(a:Int,b:Int) => (inc(inc(a)),inc_y(b)))
				var mult_moves = new ListBuffer[ListBuffer[Move]]()
				if(res != null) {
					mult_moves += ListBuffer[Move]();mult_moves(0) += new Move(from_x,from_y,res._1,res._2,"capture") 
					var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(grid(x)(y)))
					Chessboard.executeMoves(new_grid,mult_moves(0).toArray,MAX)
					checkForMultipleCapture(new_grid,res._1,res._2,inc,mult_moves(0),mult_moves)
				}
				return mult_moves
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
	def getPossibleMovesFor(grid:Array[Array[Box]],player:String,opponent:String):Array[Array[Move]] = {
		var moves = ListBuffer[Array[Move]]()
		val inc = if(player == MAX) (x:Int) => x+1 else (x:Int) => x-1
		val dec = if(player == MAX) (x:Int) => x-1 else (x:Int) => x+1
		
		/* run over all the boxes, filtering them by content and valid increment */
		grid.foreach( r => r.filter( b => b.content != null && b.content.player == player).foreach( b =>
			b.content match { 
				// Check through all possible directions
				case p : KingPawn =>  {
					val res_moves : ListBuffer[ListBuffer[Move]] = getKingPawnMove(grid,b.x,b.y,b.x+1,b.y+1,(_+1),(_+2),opponent)
					if(res_moves != null && res_moves.length > 0) res_moves.foreach(m => moves+=m.toArray)
					val res_moves2 = getKingPawnMove(grid,b.x,b.y,b.x+1,b.y-1,(_+1),(_-2),opponent)
					if(res_moves2 != null && res_moves2.length > 0) res_moves2.foreach(m => moves+=m.toArray)
					val res_moves3 = getKingPawnMove(grid,b.x,b.y,b.x-1,b.y+1,(_-1),(_+2),opponent)
					if(res_moves3 != null && res_moves3.length > 0) res_moves3.foreach(m => moves+=m.toArray)
					val res_moves4 = getKingPawnMove(grid,b.x,b.y,b.x-1,b.y-1,(_-1),(_-2),opponent)
					if(res_moves4 != null && res_moves4.length > 0) res_moves4.foreach(m => moves+=m.toArray)
				}
				// Check only in one direction
				case p : Pawn => {
					val res_moves = getPawnMove(grid,b.x,b.y,inc(b.x),b.y+1,inc,(_+2),opponent)
					// If we have possible moves, add them in moves array
					if(res_moves!=null && res_moves.length > 0) res_moves.foreach(move => moves += move.toArray)
					val res_moves2 = getPawnMove(grid,b.x,b.y,inc(b.x),b.y-1,inc,(_-2),opponent)
					// If we have possible moves, add them in moves array
					if(res_moves2!=null && res_moves2.length > 0) res_moves2.foreach(move => moves += move.toArray)
				} 
		}))
		// choose al move arrays in wich the first move is a capture
		val eat_moves = moves.filter(move => move.length > 0 && move(0).move_type == "capture")
		if(eat_moves.length > 0) {
			var length = eat_moves(0).length
			eat_moves.foreach (m => if(m.length > length) length = m.length)
			// Return all move arrays wich have the maximum lenght
			return eat_moves.filter(m => m.length == length).toArray
		}
		moves.toArray
	}
	
	/**
	 * Implements minmax algorithm. It decides the best move supposing the opponent plays in optimal mode. 
	 * If at the top level there are "capture" moves, they are the one to be considered.
	 *
	 * @param dept : maximum depth for the minmax tree of recoursive calls
	 * @param grid : represents the current situation of the chessboard
	 * @param player : the player for wich to decide the best move at the current tree level
	 * 
	 * return : a couple of elemnents, composed by the evaluation of the current state of the chessboard and the move selected
	 */	
	def minMax(depth : Int, grid : Array[Array[Box]]) : (Int,Array[Move]) = {
		return maxMove(depth,grid,Int.MaxValue,Int.MinValue)
	}
	
	def maxMove(depth : Int, game : Array[Array[Box]], alpha : Int, beta : Int) : (Int,Array[Move]) = {
		if(depth == 0) return (evaluate(MAX,game),null)
		
		val moves = getPossibleMovesFor(game,MAX,MIN)
		if(moves == null || moves.length == 0) return (evaluate(MAX,game),null)
		var best_move : (Int,Array[Move]) = null
		var new_alpha = alpha
		moves.foreach(move => {
			var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(game(x)(y)))
			Chessboard.executeMoves(new_grid,move,MAX)
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

	def minMove(depth : Int,game : Array[Array[Box]], alpha : Int, beta : Int) : (Int,Array[Move]) = {
		if(depth == 0) {
			//println("valutazione per player (depth = 0) +"+player+" : "+evaluate(player,grid))
			return (evaluate(MAX,game),null)
		}
		val moves = getPossibleMovesFor(game,MIN,MAX)
		
		if(moves == null || moves.length == 0) return (evaluate(MAX,game),null)
		
		var best_move : (Int,Array[Move]) = null
		var new_beta = beta
		moves.foreach(move => {
			// Costruisco una nuova scacchiera applicando la mossa corrente
			var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Box(game(x)(y)))
			Chessboard.executeMoves(new_grid,move,MIN)
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
	 * Very primitive heuristic function. Evaluates the goodness of the situation by giving the difference between the number of 
	 * pieces of the white player and the number of pieces of the opponent
	 * @param player : player respect who calculate the evaluation function
	 * @param grid : current chessboard situation 
	 */
	def evaluate(player:String,grid:Array[Array[Box]]) = {
		var num_player = 0
		var num_opponent = 0
		grid.foreach(row => row.foreach(c => {
			if(c.content != null && c.content.player == player){
				c.content match{
					case _:KingPawn => num_player+=2
					case _:Pawn => num_player+=1 
				}
			}
			else if(c.content != null) {
				c.content match{
					case _:KingPawn => num_opponent+=2
					case _:Pawn => num_opponent+=1 
				}
			}
		}))
		
		num_player - num_opponent
	}		
}
