import scala.swing.GridPanel
import scala.swing.FlowPanel
import scala.swing.Button
import scala.swing.BorderPanel
import scala.swing.MainFrame
import scala.swing.Label
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Dialog
import scala.swing.Window
import scala.swing.ComboBox
import scala.swing.Frame
import scala.swing.Color
import scala.swing.Separator
import scala.swing.event._

import java.io.File
import javax.swing.ImageIcon
import javax.swing.Icon
import javax.swing.SwingConstants
import javax.swing.JRootPane
import javax.swing.WindowConstants

import java.awt.Dimension
import java.awt.Image
import java.awt.Graphics2D
import javax.imageio.ImageIO

import scala.swing.Dialog._

class LoadingPopUp extends Frame {
	val i = new ImageIcon("imgs/loading.gif")
    contents = new BorderPanel(){
		add(
			new Label{
				icon = i
			}, BorderPanel.Position.Center)
    }
    peer.setAlwaysOnTop(true)
    peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)   
    def show() { visible = true; peer.requestFocus()}
    def hide() { visible = false }    	
    centerOnScreen
}

class ChessBox(bk:Icon,var top_img:Image,var selected:Boolean) extends Label{

	var empty = true
	opaque = true
	var x : Int = -1
	var y : Int = -1
	
	icon = bk
	
	def top_= (i: Image){
	    top_img = i
	    if(i!=null)empty = false
	    else empty = true
	}
	
	def top = top_img
	
	def getIconSelected = selected
	
	override def paint(g:Graphics2D) {
	    super.paint(g)
		if(top!=null)
	    	g.drawImage(top,SwingConstants.TOP, SwingConstants.TOP,null)
	}
	
}

class ChessboardView extends MainFrame {
    
	title = "Gioco della Dama"

	val loadingPopup = new LoadingPopUp

    val chessboard = new GridPanel(8,8)
    val replyButton = new Button("Reply")
    
    val depth_box = new ComboBox[Int](List(10,8,6,4,2,1))
    val eval_func = new ComboBox[String](List("dummy"))
    val heuristics = new ComboBox[String](List("none","killer heuristic"))

    depth_box.minimumSize = new Dimension(70,25)
    depth_box.maximumSize = new Dimension(70,25)
    depth_box.preferredSize = new Dimension(70,25)

    val status = new Label
    setStatus("w","White moves")
   	status.minimumSize = new Dimension(100,30)
    status.maximumSize = new Dimension(100,30)
    status.preferredSize = new Dimension(100,30)

	var white : Image = null
    var black : Image = null
    var white_selected : Image = null
    var white_king : Image = null
    var black_king : Image = null
    var white_king_selected : Image = null
    
    try{
        white = ImageIO.read(new File("imgs/w.png"))
        black = ImageIO.read(new File("imgs/b.png"))
        white_selected = ImageIO.read(new File("imgs/w_selected.png"))
        white_king = ImageIO.read(new File("imgs/ww.png"))
		black_king = ImageIO.read(new File("imgs/bb.png"))
		white_king_selected = ImageIO.read(new File("imgs/ww_selected.png"))
    }catch{
        case ex : Exception => ex.printStackTrace
    }
    var selected : ChessBox = null
    
    var boxes = Array.ofDim[ChessBox](8,8)
    
    
    try{
        javax.swing.UIManager.getInstalledLookAndFeels.foreach(info => {
            if ("Nimbus" == info.getName) javax.swing.UIManager.setLookAndFeel(info.getClassName)
        })
    }catch{
    	case ex : Exception => ex.printStackTrace
    }
    
    // creazione della scacchiera
	for(i <- 0 until 8){
		for(j <- 0 until 8){
			// Posizioni pari
			if((i+j)%2 == 0){
				boxes(i)(j) = new ChessBox(new ImageIcon("imgs/dark_80x80.jpg"),null,false);
			}else{
				boxes(i)(j) = new ChessBox(new ImageIcon("imgs/light_80x80.jpg"),null,false);
			}
			chessboard.contents += boxes(i)(j)
		}
 	}
    
 	def getDepth = depth_box.selection.item

    def setOperationForChessboard( op : (Int,Int,Int,Int) => Boolean) {
	   	for(i <- 0 until 8)
	   		for(j <- 0 until 8){
				boxes(i)(j).listenTo(boxes(i)(j).mouse.clicks)
				boxes(i)(j).reactions += {
					case (e : MouseClicked) => {
						 /* retrieve the source of the event */
				         val box = e.source.asInstanceOf[ChessBox]
				         /* if the clicked box is Empty... */
				         if(box.empty){
				         	/* if there was a box previously selected, let's check if can move */
				            if(selected != null){
				                println("sposto");
				                /* ask Game if the current move is legal and in that case, update */
				                val valid = op(selected.x,selected.y,i,j)
				            	if(valid) {
				            		// Only if the move was valid deselect the current pawn and re-enable replyButton
				            		selected = null
				            		replyButton.enabled = true
				            	}
				            }else{
				            	// no boxes previously selected 
				                println("selected è vuoto");
				            }
				            
				        }else{
				            if(box.top == black || box.top == black_king){
				                println("hai selezionato una pedina nera!!");
				                return;
				            }
				            // if the box clicked was selected, de-select it 
				            if(box.top == white_selected){
				                box.top = white
				                box.selected = false
				                selected = null
				            }else{
				            	// otherwise, select it 
				                if (box.top == white) box.top = white_selected
				                if (box.top == white_king) box.top = white_king_selected
				                box.selected = true
				                selected = box;selected.x = i;selected.y = j
				            }
				        	box.repaint
				        }   
					}
				}
			}
    }
    
   val newgame = new MenuItem("Nuova partita")

    contents = new BorderPanel{
    	add(new MenuBar {
			contents += new Menu("Menu") {		    	
		    	contents += newgame
			}
		},BorderPanel.Position.North)
    	add(chessboard, BorderPanel.Position.Center)
		//add(new FlowPanel(){contents += replyButton}, BorderPanel.Position.South)
		add(new GridPanel(2,0) {
			contents += new FlowPanel(){
				contents += new Label("Status : ")
				contents += status
				contents += new Separator
				contents += new Label("Profondità ricerca : ")
				contents += depth_box 
				
			}
			contents += new FlowPanel(){
				contents += new Label("Funzione di valutazione : ")
				contents += eval_func
				contents += new Label("Euristica : ")
				contents += heuristics
			}
		}, BorderPanel.Position.South)
    }
    
    
    def getHeuristic = heuristics.selection.item

    def setNewGameAction(op : Unit => Unit){
		newgame.listenTo(newgame)
    	newgame.reactions+={
		 	case e : ButtonClicked => {
		 		val response = scala.swing.Dialog.showConfirmation(null, "Sei sicuro?", "Nuova partita",Options.YesNo, Message.Question,scala.swing.Swing.EmptyIcon)
		 		if(response == scala.swing.Dialog.Result.Yes) op()
			}
		}
    }

    def setStatus(t:String,m : String){
    	status.text = m
    	if(t == "w") status.foreground = new Color(0,255,0) 
    	else  status.foreground = new Color(255,0,0)
    }

    /**
     * Method used to set the operation to be done when reply button is clicked
     */
    def setReplyAction(op : Unit => Unit){
    	replyButton.listenTo(replyButton)
    	replyButton.reactions += {
    		case e : ButtonClicked => {
    			op()
				replyButton.enabled = false
            }
    	}
    }
    
    /**
     * Function called to update the chessboard
     */
    def updateChessboard(chessboard : Array[Array[String]]){
        for(i <- 0 until 8){
            for(j <- 0 until 8){
                chessboard(i)(j) match {
                	case "w" => boxes(i)(j).top = white
                	case "b" => boxes(i)(j).top = black
                	case "ww" => boxes(i)(j).top = white_king
                	case "bb" => boxes(i)(j).top = black_king
                	case _ => boxes(i)(j).top = null
                }
                boxes(i)(j).repaint
            }
        }
    }

    def showPopUpMessage(message:String){
    	scala.swing.Dialog.showMessage(null, message, "info", Message.Info, scala.swing.Swing.EmptyIcon)
    }

    def showLoadingPopUp(){
    	peer.setEnabled(false)
    	loadingPopup.show
    }

    def hideLoadingPopUp(){
    	peer.setEnabled(true)
    	loadingPopup.hide
    }

    resizable = false
    centerOnScreen
    visible = true
    
}

