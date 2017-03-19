import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.util.Calendar
import scala.math._

/**
  * Class which represents the intelligent engine of the game
  */
class Engine {

  val MAX = "b"
  val MIN = "w"

  /**
	 * tells if there could be a move of type "capture" from the given position and given direction
	 */
  private def canCapture(grid: Array[Array[Square]],
                     x: Int,
                     y: Int,
                     direction: (Int, Int) => (Int, Int)): (Int, Int) = {
    val (new_x, new_y) = direction(x, y)
    if (new_x < 8 && new_y < 8 && new_y > -1 && new_x > -1 && grid(new_x)(new_y).content == null) {
      // if there's an empty cell after the opponent cell, through the given direction, say yes and return the new position
      grid(x)(y).content match {
        case p: KingPawn =>
          grid((x + new_x) / 2)((y + new_y) / 2).content match {
            case null                              =>
            case s: Pawn if (p.player != s.player) => return (new_x, new_y)
            case _                                 =>
          }
        case p: Pawn =>
          grid((x + new_x) / 2)((y + new_y) / 2).content match {
            case _: KingPawn                       =>
            case s: Pawn if (p.player != s.player) => return (new_x, new_y)
            case _                                 =>
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
  def checkForMultipleCapture(grid: Array[Array[Square]],
                              f_x: Int,
                              f_y: Int,
                              inc: Int => Int,
                              current_move: ListBuffer[Move],
                              total_moves: ListBuffer[ListBuffer[Move]]) {
    grid(f_x)(f_y).content match {
      case null =>
      case p: KingPawn => {
        val res_left     = canCapture(grid, f_x, f_y, (a: Int, b: Int) => (a + 2, b - 2))
        val res_right    = canCapture(grid, f_x, f_y, (a: Int, b: Int) => (a + 2, b + 2))
        val res_up_right = canCapture(grid, f_x, f_y, (a: Int, b: Int) => (a - 2, b + 2))
        val res_up_left  = canCapture(grid, f_x, f_y, (a: Int, b: Int) => (a - 2, b - 2))

        // If i found a legal capture "on the left" add the new move to current_move
        if (res_left != null) {
          current_move += new Move(f_x, f_y, res_left._1, res_left._2, "capture")
          var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(grid(x)(y)))
          Board.executeMoves(new_grid, current_move.toArray, MAX)
          // recoursive call
          checkForMultipleCapture(new_grid,
                                  res_left._1,
                                  res_left._2,
                                  (_ + 1),
                                  current_move,
                                  total_moves)
        }

        if (res_right != null) {
          // Create a copy of the current_move list only if there was another move on the left (separates the moves)
          var mult_moves_right: ListBuffer[Move] = null
          if (res_left == null) mult_moves_right = current_move
          else {
            // create a new ListBuffer containing common moves
            mult_moves_right = current_move.clone; mult_moves_right.trimEnd(1)
            // Add the new ListBuffer to total_moves
            total_moves += mult_moves_right
          }

          mult_moves_right += new Move(f_x, f_y, res_right._1, res_right._2, "capture")
          var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(grid(x)(y)))
          Board.executeMoves(new_grid, mult_moves_right.toArray, MAX)
          // recoursive call
          checkForMultipleCapture(new_grid,
                                  res_right._1,
                                  res_right._2,
                                  (_ + 1),
                                  mult_moves_right,
                                  total_moves)

        }

        if (res_up_left != null) {
          var mult_moves_right: ListBuffer[Move] = null
          if (res_right != null || res_left != null) {
            // create a new ListBuffer containing common moves
            mult_moves_right = current_move.clone; mult_moves_right.trimEnd(1)
            // Add the new ListBuffer to total_moves
            total_moves += mult_moves_right
          } else mult_moves_right = current_move

          mult_moves_right += new Move(f_x, f_y, res_up_left._1, res_up_left._2, "capture")
          var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(grid(x)(y)))
          Board.executeMoves(new_grid, mult_moves_right.toArray, MAX)
          // recoursive call
          checkForMultipleCapture(new_grid,
                                  res_up_left._1,
                                  res_up_left._2,
                                  (_ - 1),
                                  mult_moves_right,
                                  total_moves)
        }

        if (res_up_right != null) {
          var mult_moves_right: ListBuffer[Move] = null
          if (res_right != null || res_left != null || res_up_left != null) {
            // create a new ListBuffer containing common moves
            mult_moves_right = current_move.clone; mult_moves_right.trimEnd(1)
            // Add the new ListBuffer to total_moves
            total_moves += mult_moves_right
          } else mult_moves_right = current_move

          mult_moves_right += new Move(f_x, f_y, res_up_right._1, res_up_right._2, "capture")
          var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(grid(x)(y)))
          Board.executeMoves(new_grid, mult_moves_right.toArray, MAX)
          // recoursive call
          checkForMultipleCapture(new_grid,
                                  res_up_right._1,
                                  res_up_right._2,
                                  (_ - 1),
                                  mult_moves_right,
                                  total_moves)
        }
      }
      case p: Pawn => {
        val res_left  = canCapture(grid, f_x, f_y, (a: Int, b: Int) => (inc(inc(a)), b - 2))
        val res_right = canCapture(grid, f_x, f_y, (a: Int, b: Int) => (inc(inc(a)), b + 2))

        // If i found a legal capture "on the left" add the new move to current_move
        if (res_left != null) {
          current_move += new Move(f_x, f_y, res_left._1, res_left._2, "capture")
          var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(grid(x)(y)))
          Board.executeMoves(new_grid, current_move.toArray, MAX)
          // recoursive call
          checkForMultipleCapture(new_grid,
                                  res_left._1,
                                  res_left._2,
                                  inc,
                                  current_move,
                                  total_moves)
        }

        if (res_right != null) {
          // Create a copy of the current_move list only if there was another move on the left (separates the moves)
          var mult_moves_right: ListBuffer[Move] = null
          if (res_left == null) mult_moves_right = current_move
          else {
            // create a new ListBuffer containing common moves
            mult_moves_right = current_move.clone; mult_moves_right.trimEnd(1)
            // Add the new ListBuffer to total_moves
            total_moves += mult_moves_right
          }

          mult_moves_right += new Move(f_x, f_y, res_right._1, res_right._2, "capture")
          var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(grid(x)(y)))
          Board.executeMoves(new_grid, mult_moves_right.toArray, MAX)
          // recoursive call
          checkForMultipleCapture(new_grid,
                                  res_right._1,
                                  res_right._2,
                                  inc,
                                  mult_moves_right,
                                  total_moves)
        }
      }
    }
  }

  /**
	 * Creates the move for a simple pawngiven from-coordinates and to-coordiantes. It checks if there is a legal "capture" move.
	 *
	 * @param grid : current configuration
	 * @param form_x,from_y : from-coordinates
	 * @param x,y : to-coordiantes
	 * @param inc : function wich increments properly the x coordinate
	 * @param inc_y : function used to increment the y coordinate when searching for an "capture" move
	 */
  private def getPawnMove(grid: Array[Array[Square]],
                          from_x: Int,
                          from_y: Int,
                          x: Int,
                          y: Int,
                          inc: Int => Int,
                          inc_y: Int => Int): ListBuffer[ListBuffer[Move]] = {
    /* look at what there is in the near square */
    if (x < 0 || x > 7 || y < 0 || y > 7) return null
    grid(x)(y).content match {
      case null =>
        val b = new ListBuffer[ListBuffer[Move]]();
        b += ListBuffer(new Move(from_x, from_y, x, y, "move")); return b
      case p: KingPawn =>
      // Case in wich there is a simple opponent pawn
      case p: Pawn => {
        var res = canCapture(grid, from_x, from_y, (a: Int, b: Int) => (inc(inc(a)), inc_y(b)))
        var t   = new ListBuffer[ListBuffer[Move]]()
        if (res != null) {
          t += new ListBuffer[Move](); t(0) += new Move(from_x, from_y, res._1, res._2, "capture")
          var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(grid(x)(y)))
          Board.executeMoves(new_grid, t(0).toArray, MAX)
          checkForMultipleCapture(new_grid, res._1, res._2, inc, t(0), t)
        }
        return t
      }
      case _ =>
    }
    null
  }

  /**
	 * Creates the move for a king pawn given from-coordinates and to-coordiantes. It checks if there is a legal "capture" move.
	 *
	 * @param grid : current configuration
	 * @param form_x,from_y : from-coordinates
	 * @param x,y : to-coordiantes
	 * @param inc : function wich increments properly the x coordinate
	 * @param inc_y : function used to increment the y coordinate when searching for an "capture" move
	 */
  private def getKingPawnMove(grid: Array[Array[Square]],
                              from_x: Int,
                              from_y: Int,
                              x: Int,
                              y: Int,
                              inc: Int => Int,
                              inc_y: Int => Int,
                              opponent: String): ListBuffer[ListBuffer[Move]] = {
    /* look at what there is in the near square */
    if (x < 0 || x > 7 || y < 0 || y > 7) return null
    grid(x)(y).content match {
      case null =>
        val b = new ListBuffer[ListBuffer[Move]]();
        b += ListBuffer(new Move(from_x, from_y, x, y, "move")); return b
      // Every pawn is ok!
      case p: Pawn => {
        val res        = canCapture(grid, from_x, from_y, (a: Int, b: Int) => (inc(inc(a)), inc_y(b)))
        var mult_moves = new ListBuffer[ListBuffer[Move]]()
        if (res != null) {
          mult_moves += ListBuffer[Move]();
          mult_moves(0) += new Move(from_x, from_y, res._1, res._2, "capture")
          var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(grid(x)(y)))
          Board.executeMoves(new_grid, mult_moves(0).toArray, MAX)
          checkForMultipleCapture(new_grid, res._1, res._2, inc, mult_moves(0), mult_moves)
        }
        return mult_moves
      }
      case _ =>
    }
    null
  }

  /**
	 * Given a configuration (grid), a player and his opponent, returns all the legal moves for the current player
	 *
	 * @param grid : configuration
	 * @param player : current player
	 * @param opponent : the opponent
	 *
	 * return all legal moves from the given configuration
	 */
  def getLegalMovesFor(grid: Array[Array[Square]],
                          player: String,
                          opponent: String): Array[Array[Move]] = {
    var moves = ListBuffer[Array[Move]]()
    val inc   = if (player == MAX) (x: Int) => x + 1 else (x: Int) => x - 1
    val dec   = if (player == MAX) (x: Int) => x - 1 else (x: Int) => x + 1

    /* run over all the squares, filtering them by content and valid increment */
    grid.foreach(
      r =>
        r.filter(b => b.content != null && b.content.player == player)
          .foreach(b =>
            b.content match {
              // Check through all possible directions
              case p: KingPawn => {
                val res_moves: ListBuffer[ListBuffer[Move]] =
                  getKingPawnMove(grid, b.x, b.y, b.x + 1, b.y + 1, (_ + 1), (_ + 2), opponent)
                if (res_moves != null && res_moves.length > 0)
                  res_moves.foreach(m => moves += m.toArray)
                val res_moves2 =
                  getKingPawnMove(grid, b.x, b.y, b.x + 1, b.y - 1, (_ + 1), (_ - 2), opponent)
                if (res_moves2 != null && res_moves2.length > 0)
                  res_moves2.foreach(m => moves += m.toArray)
                val res_moves3 =
                  getKingPawnMove(grid, b.x, b.y, b.x - 1, b.y + 1, (_ - 1), (_ + 2), opponent)
                if (res_moves3 != null && res_moves3.length > 0)
                  res_moves3.foreach(m => moves += m.toArray)
                val res_moves4 =
                  getKingPawnMove(grid, b.x, b.y, b.x - 1, b.y - 1, (_ - 1), (_ - 2), opponent)
                if (res_moves4 != null && res_moves4.length > 0)
                  res_moves4.foreach(m => moves += m.toArray)
              }
              // Check only in one direction
              case p: Pawn => {
                val res_moves = getPawnMove(grid, b.x, b.y, inc(b.x), b.y + 1, inc, (_ + 2))
                // If we have legal moves, add them in moves array
                if (res_moves != null && res_moves.length > 0)
                  res_moves.foreach(move => moves += move.toArray)
                val res_moves2 = getPawnMove(grid, b.x, b.y, inc(b.x), b.y - 1, inc, (_ - 2))
                // If we have legal moves, add them in moves array
                if (res_moves2 != null && res_moves2.length > 0)
                  res_moves2.foreach(move => moves += move.toArray)
              }
          }))
    // choose al move arrays in wich the first move is a capture
    val capture_moves = moves.filter(move => move.length > 0 && move(0).move_type == "capture")
    if (capture_moves.length > 0) {
      var length = capture_moves(0).length
      capture_moves.foreach(m => if (m.length > length) length = m.length)
      // Return all move arrays wich have the maximum lenght
      return capture_moves.filter(m => m.length == length).toArray
    }
    moves.toArray
  }

  var best_moves_database = new ListBuffer[Array[Move]]()
  var nodes               = 0

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
  def minimax(depth: Int,
             grid: Array[Array[Square]],
             killerHeuristic: Boolean,
             eval: String): (Int, Array[Move]) = {
    nodes = 0
    if (killerHeuristic) {
      maxMove(1, grid, Int.MinValue, Int.MaxValue, eval)
      for (i <- 2 until depth) {
        maxMove(i, grid, Int.MinValue, Int.MaxValue, eval)
        println("depth = " + i)
        best_moves_database.foreach(m => { m.foreach(a => a.printMove); println })
        println("Nodi visitati = " + nodes)
      }
    }
    val res = maxMove(depth, grid, Int.MinValue, Int.MaxValue, eval)
    println("Nodi visitati = " + nodes)
    return res
  }

  def maxMove(depth: Int,
              game: Array[Array[Square]],
              alpha: Int,
              beta: Int,
              eval: String): (Int, Array[Move]) = {
    if (depth == 0) return (evaluate(MAX, game, eval, MAX), null)

    val moves = getLegalMovesFor(game, MAX, MIN)
    if (moves == null || moves.length == 0) return (evaluate(MAX, game, eval, MAX), null)
    var best_move: (Int, Array[Move]) = null
    var new_alpha                     = alpha

    if (!(best_moves_database isEmpty)) {
      // TODO : porto in prima posizione la mossa che corrisponde a best_moves_database(0) usare sameElements
      for (i <- 0 until moves.length) {
        if (moves(i).sameElements(best_moves_database(0))) {
          val a = moves(0)
          moves(0) = best_moves_database(0)
          moves(i) = a
        }
      }
      best_moves_database = best_moves_database.drop(1)
    }

    moves.foreach(move => {
      nodes += 1
      var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(game(x)(y)))
      Board.executeMoves(new_grid, move, MAX)
      // tocca a Min!
      val min_move = minMove(depth - 1, new_grid, new_alpha, beta, eval)

      if (best_move == null || best_move._1 < min_move._1) {
        best_move = (min_move._1, move)
        new_alpha = max(new_alpha, best_move._1)
      }

      if (best_move._1 >= beta) {
        // Returns because no better moves could be choose
        best_moves_database.prepend(best_move._2)
        return best_move
      }
    })
    best_moves_database.prepend(best_move._2)
    return best_move
  }

  def minMove(depth: Int,
              game: Array[Array[Square]],
              alpha: Int,
              beta: Int,
              eval: String): (Int, Array[Move]) = {
    if (depth == 0) return (evaluate(MAX, game, eval, MIN), null)
    val moves = getLegalMovesFor(game, MIN, MAX)

    if (moves == null || moves.length == 0) return (evaluate(MAX, game, eval, MIN), null)

    var best_move: (Int, Array[Move]) = null
    var new_beta                      = beta

    if (!(best_moves_database isEmpty)) {
      // TODO : porto in prima posizione la mossa che corrisponde a best_moves_database(0) usare sameElements
      for (i <- 0 until moves.length) {
        if (moves(i).sameElements(best_moves_database(0))) {
          val a = moves(0)
          moves(0) = best_moves_database(0)
          moves(i) = a
        }
      }
      best_moves_database = best_moves_database.drop(1)
    }

    moves.foreach(move => {
      // Costruisco una nuova scacchiera applicando la mossa corrente
      nodes += 1
      var new_grid = Array.tabulate(8, 8)((x: Int, y: Int) => new Square(game(x)(y)))
      Board.executeMoves(new_grid, move, MIN)
      // tocca a Min!
      val max_move = maxMove(depth - 1, new_grid, alpha, new_beta, eval)
      if (best_move == null || best_move._1 > max_move._1) {
        best_move = (max_move._1, move)
        new_beta = min(new_beta, best_move._1)
      }
      if (best_move._1 <= alpha) {
        // No moves better then this will be choosen by maxMoves, so is worthless to continue
        best_moves_database.prepend(best_move._2)
        return best_move
      }
    })
    best_moves_database.prepend(best_move._2)
    return best_move
  }

  /*def minimax(depth : Int, grid : Array[Array[Square]],k:Boolean,s:String) : (Int,Array[Move]) = {
		return maxMove(depth,grid,Int.MaxValue,Int.MinValue)
	}
	
	def maxMove(depth : Int, game : Array[Array[Square]], alpha : Int, beta : Int) : (Int,Array[Move]) = {
		if(depth == 0) return (evaluate2(MAX,game),null)
		
		val moves = getLegalMovesFor(game,MAX,MIN)
		if(moves == null || moves.length == 0) return (evaluate2(MAX,game),null)
		var best_move : (Int,Array[Move]) = null
		var new_alpha = alpha
		moves.foreach(move => {
			var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Square(game(x)(y)))
			Board.executeMoves(new_grid,move,MAX)
			// tocca a Min!
			val min_move = minMove(depth-1,new_grid,new_alpha,beta)

			if(best_move == null || best_move._1 < min_move._1){
				best_move = (min_move._1,move)
				new_alpha = best_move._1
			}
			if(beta > new_alpha && best_move!=null) {
				return best_move
			}
		})
		return best_move
	}

	def minMove(depth : Int,game : Array[Array[Square]], alpha : Int, beta : Int) : (Int,Array[Move]) = {
		if(depth == 0) {
			//println("valutazione per player (depth = 0) +"+player+" : "+evaluate2(player,grid))
			return (evaluate2(MAX,game),null)
		}
		val moves = getLegalMovesFor(game,MIN,MAX)
		
		if(moves == null || moves.length == 0) return (evaluate2(MAX,game),null)
		
		var best_move : (Int,Array[Move]) = null
		var new_beta = beta
		moves.foreach(move => {
			// Costruisco una nuova scacchiera applicando la mossa corrente
			var new_grid = Array.tabulate(8,8)((x:Int,y:Int) => new Square(game(x)(y)))
			Board.executeMoves(new_grid,move,MIN)
			// tocca a Min!
			val max_move = maxMove(depth-1,new_grid,alpha,new_beta)
			if(best_move == null || best_move._1 > max_move._1){
				best_move = (max_move._1,move)
				new_beta = best_move._1
			}
			if(new_beta < alpha && best_move!=null) {
				return best_move
			}
		})
		return best_move
	}
	*/

  def evaluate(player: String, grid: Array[Array[Square]], eval: String, inGame: String) = {
    eval match {
      case "dummy" => evaluate1(player, grid)
      case "eval2" => evaluate2(player, grid)
      case "eval3" => evaluate3(player, grid)
      case "eval4" => evaluate4(player, grid, inGame)
      case _       => evaluate1(player, grid)
    }
  }

  /**
	 * Very primitive heuristic function. Evaluates the goodness of the situation by giving the difference between the number of
	 * pieces of the white player and the number of pieces of the opponent
	 * @param player : player respect who calculate the evaluation function
	 * @param grid : current chessboard situation
	 */
  def evaluate1(player: String, grid: Array[Array[Square]]) = {
    var num_player   = 0
    var num_opponent = 0
    grid.foreach(row =>
      row.foreach(c => {
        if (c.content != null && c.content.player == player) {
          c.content match {
            case _: KingPawn => num_player += 2
            case _: Pawn     => num_player += 1
          }
        } else if (c.content != null) {
          c.content match {
            case _: KingPawn => num_opponent += 2
            case _: Pawn     => num_opponent += 1
          }
        }
      }))

    num_player - num_opponent
  }

  def evaluate2(player: String, grid: Array[Array[Square]]) = {
    var score = 0
    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        grid(i)(j).content match {
          case null =>
          case p: KingPawn =>
            if (p.player == player) {
              score += Engine.KINGPAWN
              if (i == 0 || i == 7) score -= Engine.EDGE
              if (j == 0 || j == 7) score -= Engine.EDGE
            } else {
              score -= Engine.KINGPAWN
              if (i == 0 || i == 7) score += Engine.EDGE
              if (j == 0 || j == 7) score += Engine.EDGE
            }
          case p: Pawn =>
            if (p.player == player) {
              score += Engine.PAWN
              score += Engine.POS * i * i
            } else {
              score -= Engine.PAWN
              score -= Engine.POS * (7 - i) * (7 - i)
            }
        }
      }
    }
    score += (new Random(Calendar.getInstance().getTimeInMillis())
      .nextInt(Engine.RANDOM_WEIGHT))
    score
  }

  def minSupport(player: String, grid: Array[Array[Square]], y: Int, x: Int): Int = {
    var score = 0
    var iR    = 0
    if (!Board.isMoveLegal(x, y) || ((Board.isMoveLegal(y + 1, x - 1) && grid(y + 1)(
          x - 1).content != null && grid(y + 1)(x - 1).content.player != player) && (Board.isMoveLegal(
          y + 1,
          x + 1) && grid(y + 1)(x + 1).content != null && grid(y + 1)(x + 1).content.player != player)))
      if (y == 8) 0
      else -1
    else {
      iR = minSupport(player, grid, y + 1, x - 1)
      if (iR != -1) score += iR + 2;
      iR = minSupport(player, grid, y + 1, x + 1);
      if (iR != -1) score += iR + 2;
    }
    score
  }

  def maxSupport(player: String, grid: Array[Array[Square]], y: Int, x: Int): Int = {
    var score = 0
    var iR    = 0
    if (!Board.isMoveLegal(y, x) || ((Board.isMoveLegal(y - 1, x - 1) && grid(y - 1)(
          x - 1).content != null && grid(y - 1)(x - 1).content.player == player) && (Board.isMoveLegal(
          y - 1,
          x + 1) && grid(y - 1)(x + 1).content != null && grid(y - 1)(x + 1).content.player == player)))
      if (y == -1) 0
      else -1
    else {
      iR = maxSupport(player, grid, y - 1, x - 1)
      if (iR != -1) score += iR + 2;
      iR = maxSupport(player, grid, y - 1, x + 1);
      if (iR != -1) score += iR + 2;
    }
    score
  }

  def evaluate3(player: String, grid: Array[Array[Square]]) = {
    var score = 0
    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        grid(i)(j).content match {
          case null =>
          case p: KingPawn =>
            if (p.player == player) {
              score += Engine.KINGPAWN
              if (i == 0 || i == 7) score -= Engine.EDGE
              if (j == 0 || j == 7) score -= Engine.EDGE
            } else {
              score -= Engine.KINGPAWN
              if (i == 0 || i == 7) score += Engine.EDGE
              if (j == 0 || j == 7) score += Engine.EDGE
            }
          case p: Pawn =>
            if (p.player == player) {
              score += Engine.PAWN
              score += Engine.POS * i * i
              score += maxSupport(player, grid, i, j)
            } else {
              score -= Engine.PAWN
              score -= Engine.POS * (7 - i) * (7 - i)
              score -= minSupport(player, grid, i, j)
            }
        }
      }
    }
    score += (new Random(Calendar.getInstance().getTimeInMillis()).nextInt(Engine.MOVE))
    score
  }

  def evaluate4(player: String, grid: Array[Array[Square]], inGame: String) = {
    var pawnFound = false
    var kingFound = false
    var pawnCount = 0
    var score     = 0
    for (i <- 0 until 8 if (!kingFound)) {
      for (j <- 0 until 8 if (!kingFound)) {
        grid(i)(j).content match {
          case null =>
          case p: KingPawn =>
            pawnCount += 1
            kingFound = true
          case p: Pawn => pawnCount += 1
        }
      }
    }
    if (!kingFound || pawnCount > Engine.MAX_PAWN) {
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          grid(i)(j).content match {
            case null =>
            case p: Pawn =>
              if (p.player == player) {
                if (i <= 3) score += Engine.PAWN * (8 - j) * (8 - j) * i * i
                else score += Engine.PAWN * j * j * i * i
              } else {
                if ((7 - i) <= 5)
                  score -= Engine.PAWN * (8 - j) * (8 - j) * (7 - i) * (7 - i)
                else score -= Engine.PAWN * j * j * (7 - i) * (7 - i)
              }
          }
        }
      }
    } else {
      var row             = 0
      var column          = 0
      var pairCoupleCount = 0
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          grid(i)(j).content match {
            case null =>
            case p: KingPawn =>
              if (p.player == player) score += Engine.KINGPAWN
              else score -= Engine.KINGPAWN
            case p: Pawn =>
              if (!pawnFound) {
                row = i
                column = j
                pawnFound = true
              } else {
                pawnFound = false
                if (scala.math.max(scala.math.abs(row - i), scala.math.abs(column - j)) % 2 == 0)
                  pairCoupleCount += 1
              }
              if (p.player == player) score += Engine.PAWN * i * i
              else score -= Engine.PAWN * (7 - i) * (7 - i)
          }
        }
      }
      if ((pairCoupleCount % 2) == 1) {
        if (inGame == MAX) score += Engine.MOVE
        else score -= Engine.MOVE
      }
    }
    score += (new Random(Calendar.getInstance().getTimeInMillis())
      .nextInt(Engine.RANDOM_WEIGHT))
    score
  }

  object Engine {
    val PAWN          = 100
    val KINGPAWN      = 200
    val MOVE          = 20
    val POS           = 1
    val EDGE          = 10
    val RANDOM_WEIGHT = 10
    val MAX_PAWN      = 18
  }
}
