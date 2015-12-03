import java.awt.{Color, Graphics2D, Dimension}
import scala.swing._

object SecondSwingApp extends SimpleSwingApplication {
  def top = new MainFrame {

    val generator = new Maze3D(20, 20, 1)
    val maze = generator.build

    //maze(maze.length-1)(maze(maze.length-1).length-1)(maze(maze(maze.length-1).length-1).length-1).setOpenEst
    maze(0)(0)(0).setOpenWest
	
	var solver = new DeadEndFiller(maze)
	solver.findDeadEnds

	var widthApp  = 700
	var heightApp = 700

	var widthCell = widthApp/(maze(0)(0).length+1) + 1
	var heightCell = heightApp/(maze(0).length+1) + 1

	var widthBloc = widthCell/2
	var heightBloc = heightCell/2

	var widthScreen  = widthApp - widthBloc
	var heightScreen = heightApp - heightBloc
    var button1 = new Button {
      text = "Click me"
    }

    contents = new DataPanel(maze, widthCell, heightCell, widthBloc, heightBloc) {
      preferredSize = new Dimension(widthScreen, heightScreen)
      // contents += button1
    }
    

    //add button to panel
    

    // listenTo(button1) 
    // reactions += {
    //   case ButtonClicked(_) => print("ok")
    // } 
  }
}  

class DataPanel(maze: Array[Array[Array[Cell]]], widthCell: Int, heightCell: Int, widthBloc: Int, heightBloc: Int) extends Panel {

  override def paintComponent(g: Graphics2D) {
    
      drawFloor(g, maze(0), widthCell, heightCell, widthBloc, heightBloc)
  }

  def drawFloor(g: Graphics2D, floor: Array[Array[Cell]], widthCell: Int, heightCell: Int, widthBloc: Int, heightBloc: Int) {
      var x = 0
      var y = 0
      var i = 0
      var j = 0
      g.setColor(Color.BLACK)
      for(j <- 0 to floor.length-1){
      	x = 0
      	for(i <- 0 to floor(j).length-1){
	      	if(i == floor(j).length-1) g.fillRect(x + widthCell, y, widthBloc, heightCell)
	      	if(j == floor.length-1)    g.fillRect(x, y + heightCell, widthCell, heightBloc)
      		drawCell(g, x, y, floor(j)(i), widthBloc, heightBloc)
      		x = x + widthCell
      	}
      	y = y + heightCell
      }
	  g.fillRect(x, y, widthBloc, heightBloc)
  }

  def drawCell(g: Graphics2D, x: Int, y: Int, cell: Cell, widthBlock: Int, heightBlock: Int) {
  	var i = 0
	g.fillRect(x, y, widthBlock, heightBlock)
	if(!cell.openNorth) g.fillRect(x + widthBlock, y, widthBlock, heightBlock)
	if(!cell.openWest) g.fillRect(x, y + heightBlock, widthBlock, heightBlock)
	if(cell.marked) {
		g.setColor(Color.YELLOW)
		g.fillRect(x + widthBlock, y + heightBlock, widthBlock, heightBlock)
		g.setColor(Color.BLACK)
	}
	if(cell.markedTest) {
		g.setColor(Color.RED)
		g.fillRect(x + widthBlock, y + heightBlock, widthBlock, heightBlock)
		g.setColor(Color.BLACK)
	}
  }
}