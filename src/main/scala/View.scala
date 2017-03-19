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
import java.util.Locale;
import java.util.ResourceBundle;

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
  val i = new ImageIcon(getClass.getResource("/imgs/loading.gif"))
  contents = new BorderPanel() {
    add(new Label {
      icon = i
    }, BorderPanel.Position.Center)
  }
  peer.setAlwaysOnTop(true)
  peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
  def show() { visible = true; peer.requestFocus() }
  def hide() { visible = false }
  centerOnScreen
}

class BoardSquare(bk: Icon, var top_img: Image, var selected: Boolean) extends Label {

  var empty = true
  opaque = true
  var x: Int = -1
  var y: Int = -1

  icon = bk

  def top_=(i: Image) {
    top_img = i
    if (i != null) empty = false
    else empty = true
  }

  def top = top_img

  def getIconSelected = selected

  override def paint(g: Graphics2D) {
    super.paint(g)
    if (top != null)
      g.drawImage(top, SwingConstants.TOP, SwingConstants.TOP, null)
  }

}

class BoardView extends MainFrame {
  var currentLocale = Locale.getDefault();
  var messages = ResourceBundle.getBundle("MessagesBundle", currentLocale);
  
  title = messages.getString("windowTitle");

  val loadingPopup = new LoadingPopUp

  val board  = new GridPanel(8, 8)
  val replyButton = new Button(messages.getString("reply"))

  val depth_box  = new ComboBox[Int](List(10, 8, 6, 4, 2, 1))
  val eval_func  = new ComboBox[String](List("dummy", "eval2", "eval3", "eval4"))
  val heuristics = new ComboBox[String](List("none", "killer heuristic"))

  depth_box.minimumSize = new Dimension(70, 25)
  depth_box.maximumSize = new Dimension(70, 25)
  depth_box.preferredSize = new Dimension(70, 25)

  val status = new Label
  setStatus("w", messages.getString("whiteMoves"))
  status.minimumSize = new Dimension(100, 30)
  status.maximumSize = new Dimension(100, 30)
  status.preferredSize = new Dimension(100, 30)

  var white: Image               = null
  var black: Image               = null
  var white_selected: Image      = null
  var white_king: Image          = null
  var black_king: Image          = null
  var white_king_selected: Image = null

  try {
    white = ImageIO.read(getClass.getResource("/imgs/w.png"))
    black = ImageIO.read(getClass.getResource("/imgs/b.png"))
    white_selected = ImageIO.read(getClass.getResource("/imgs/w_selected.png"))
    white_king = ImageIO.read(getClass.getResource("/imgs/ww.png"))
    black_king = ImageIO.read(getClass.getResource("/imgs/bb.png"))
    white_king_selected = ImageIO.read(getClass.getResource("/imgs/ww_selected.png"))
  } catch {
    case ex: Exception => ex.printStackTrace
  }
  var selected: BoardSquare = null

  var squares = Array.ofDim[BoardSquare](8, 8)

  // creazione della scacchiera
  for (i <- 0 until 8) {
    for (j <- 0 until 8) {
      // Posizioni pari
      if ((i + j) % 2 == 0) {
        squares(i)(j) =
          new BoardSquare(new ImageIcon(getClass.getResource("/imgs/dark_80x80.jpg")), null, false);
      } else {
        squares(i)(j) =
          new BoardSquare(new ImageIcon(getClass.getResource("/imgs/light_80x80.jpg")), null, false);
      }
      board.contents += squares(i)(j)
    }
  }

  def getDepth = depth_box.selection.item

  def setOperationForBoard(op: (Int, Int, Int, Int) => Boolean) {
    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        squares(i)(j).listenTo(squares(i)(j).mouse.clicks)
        squares(i)(j).reactions += {
          case (e: MouseClicked) => {
            /* retrieve the source of the event */
            val square = e.source.asInstanceOf[BoardSquare]
            /* if the clicked square is Empty... */
            if (square.empty) {
              /* if there was a square previously selected, let's check if can move */
              if (selected != null) {
                println("sposto");
                /* ask Game if the current move is legal and in that case, update */
                val valid = op(selected.x, selected.y, i, j)
                if (valid) {
                  // Only if the move was valid deselect the current pawn and re-enable replyButton
                  selected = null
                  replyButton.enabled = true
                }
              } else {
                // no squares previously selected
                println(messages.getString("emptySquareSelected"));
              }

            } else {
              if (square.top == black || square.top == black_king) {                
                println(messages.getString("pickedBlackPiece"));
                return;
              }
              // if the square clicked was selected, de-select it
              if (square.top == white_selected) {
                square.top = white
                square.selected = false
                selected = null
              } else {
                // otherwise, select it
                if (square.top == white) square.top = white_selected
                if (square.top == white_king) square.top = white_king_selected
                square.selected = true
                selected = square; selected.x = i; selected.y = j
              }
              square.repaint
            }
          }
        }
      }
    }
  }

  val newgame = new MenuItem(messages.getString("newGame"))

  contents = new BorderPanel {
    add(new MenuBar {
      contents += new Menu(messages.getString("menu")) {
        contents += newgame
      }
    }, BorderPanel.Position.North)
    add(board, BorderPanel.Position.Center)
    //add(new FlowPanel(){contents += replyButton}, BorderPanel.Position.South)
    add(
      new GridPanel(2, 0) {
        contents += new FlowPanel() {
          contents += new Label(messages.getString("status"))
          contents += status
          contents += new Separator
          contents += new Label(messages.getString("searchDepth"))
          contents += depth_box

        }
        contents += new FlowPanel() {
          contents += new Label(messages.getString("evalFunction"))
          contents += eval_func
          contents += new Label(messages.getString("heuristics"))
          contents += heuristics
        }
      },
      BorderPanel.Position.South
    )
  }

  def getHeuristic = heuristics.selection.item
  def getEval      = eval_func.selection.item

  def setNewGameAction(op: Unit => Unit) {
    newgame.listenTo(newgame)
    newgame.reactions += {
      case e: ButtonClicked => {
        val response = scala.swing.Dialog.showConfirmation(null,
                                                           messages.getString("really"),
                                                           messages.getString("newGame"),
                                                           Options.YesNo,
                                                           Message.Question,
                                                           scala.swing.Swing.EmptyIcon)
        if (response == scala.swing.Dialog.Result.Yes) op()
      }
    }
  }

  def setStatus(t: String, m: String) {
    status.text = m
    if (t == "w") status.foreground = new Color(0, 255, 0)
    else status.foreground = new Color(255, 0, 0)
  }

  /**
    * Method used to set the operation to be done when reply button is clicked
    */
  def setReplyAction(op: Unit => Unit) {
    replyButton.listenTo(replyButton)
    replyButton.reactions += {
      case e: ButtonClicked => {
        op()
        replyButton.enabled = false
      }
    }
  }

  /**
    * Function called to update the board
    */
  def updateBoard(board: Array[Array[String]]) {
    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        board(i)(j) match {
          case "w"  => squares(i)(j).top = white
          case "b"  => squares(i)(j).top = black
          case "ww" => squares(i)(j).top = white_king
          case "bb" => squares(i)(j).top = black_king
          case _    => squares(i)(j).top = null
        }
        squares(i)(j).repaint
      }
    }
  }

  def showPopUpMessage(message: String) {
    scala.swing.Dialog.showMessage(null,
                                   message,
                                   "info",
                                   Message.Info,
                                   scala.swing.Swing.EmptyIcon)
  }

  def showLoadingPopUp() {
    peer.setEnabled(false)
    loadingPopup.show
  }

  def hideLoadingPopUp() {
    peer.setEnabled(true)
    loadingPopup.hide
  }

  resizable = false
  centerOnScreen
  visible = true

}
