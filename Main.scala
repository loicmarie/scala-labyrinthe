import java.awt.{Color, Graphics2D, Dimension}
import scala.swing._
import scala.swing.event._

object SecondSwingApp extends SimpleSwingApplication {
  def top = new MainFrame {

    val generator = new Maze3D(15, 15, 3)
    var maze = generator.build

    var depth = 0

    //maze(maze.length-1)(maze(maze.length-1).length-1)(maze(maze(maze.length-1).length-1).length-1).setOpenEst
    maze(0)(0)(0).setOpenWest
    var mazeCopy = copyMaze(maze)

  var widthApp  = 1000.0
  var heightApp = 800.0

  var widthCell = widthApp/(maze(0)(0).length)
  var heightCell = heightApp/(maze(0).length)

  var widthBloc = widthCell/2
  var heightBloc = heightCell/2

  var widthScreen  = widthApp + widthBloc
  var heightScreen = heightApp + heightBloc

    var panel = new DataPanel(maze, widthCell, heightCell, widthBloc, heightBloc, depth) {
      preferredSize = new Dimension(widthScreen.toInt, heightScreen.toInt)
    }

    val button2 = Button("Recursive Backtracking")(recursiveBacktracking())
    val button3 = Button("Dead End Filler")(deadEndFiller())
    val button4 = Button("Reset")(reset())
    val button5 = Button("-1")(changeDepth(-1))
    val button6 = Button("+1")(changeDepth(1))
    val button = Button("Maze generation")(generate())

    updateView()

    def generate() {
      maze = generator.build
      maze(0)(0)(0).setOpenWest
      mazeCopy = copyMaze(maze)
      updateView()
    }

    def deadEndFiller() {
      reset
      var mazeCopy1 = copyMaze(mazeCopy)
      var solver1 = new DeadEndFiller(copyMaze(mazeCopy), mazeCopy1)
      solver1.init
      maze = solver1.getSolution
      updateView()
    }

    def recursiveBacktracking() {
      reset
      var mazeCopy2 = copyMaze(mazeCopy)
      var solver2 = new RecursiveBacktracking(copyMaze(mazeCopy), mazeCopy2)
      solver2.init
      maze = solver2.getSolution
      updateView()
    }

    def updateView(){
      panel = new DataPanel(maze, widthCell, heightCell, widthBloc, heightBloc, depth) {
        preferredSize = new Dimension(widthScreen.toInt, heightScreen.toInt)
      }
      contents = new BoxPanel(Orientation.Vertical) {
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += button
          contents += button2
          contents += button3
          contents += button4
          contents += button5
          contents += button6
        }
        contents += panel
      }
      panel.repaint()
    }

    def changeDepth(dz: Int) {
      depth = depth + dz
      updateView()
    }

    def reset() {
      var x = 0
      var y = 0
      var z = 0
      for(z <- 0 to maze.length-1){
        for(y <- 0 to maze(z).length-1){
          for(x <- 0 to maze(z)(y).length-1){
            if(maze(z)(y)(x).marked) maze(z)(y)(x).unmark
            if(maze(z)(y)(x).markedTest) maze(z)(y)(x).unmarkTest
          }
        }
      }
      updateView()
    }
  }


  def copyMaze(maze: Array[Array[Array[Cell]]]): Array[Array[Array[Cell]]] = {
    var x = 0
    var y = 0
    var z = 0
    var num = 0
    var mazeCopy = Array.ofDim[Cell](maze.length,maze(0).length, maze(0)(0).length)
    for(z <- 0 to maze.length-1){
      for(y <- 0 to maze(z).length-1){
        for(x <- 0 to maze(z)(y).length-1){
          mazeCopy(z)(y)(x) = maze(z)(y)(x).copy
        }
      }
    }
    mazeCopy
  }
}  

class DataPanel(maze: Array[Array[Array[Cell]]], widthCell: Double, heightCell: Double, widthBloc: Double, heightBloc: Double, currentDepth: Int) extends Panel {

  override def paintComponent(g: Graphics2D) {
      if(currentDepth < maze.length && currentDepth >= 0){
        drawFloor(g, maze(currentDepth), widthCell, heightCell, widthBloc, heightBloc)
      }
  }

  def drawFloor(g: Graphics2D, floor: Array[Array[Cell]], widthCell: Double, heightCell: Double, widthBloc: Double, heightBloc: Double) {
      var x = 0.0
      var y = 0.0
      var i = 0
      var j = 0
      var openDown = false
      g.setColor(Color.BLACK)
      for(j <- 0 to floor.length-1){
      	x = 0.0
      	for(i <- 0 to floor(j).length-1){
	      	if(i == floor(j).length-1) g.fill(new Rectangle((x + widthCell).toInt, y.toInt, (widthBloc+1).toInt, (heightCell+1).toInt))
	      	if(j == floor.length-1)    g.fill(new Rectangle(x.toInt, (y + heightCell).toInt, (widthCell+1).toInt, (heightBloc+1).toInt))
      		if(currentDepth-1 > 0) openDown = maze(currentDepth-1)(j)(i).openUp
          else openDown = false
          drawCell(g, x, y, floor(j)(i), openDown, widthBloc, heightBloc)
      		x = x + widthCell
      	}
      	y = y + heightCell
      }
	  g.fill(new Rectangle(x.toInt, y.toInt, (widthBloc+1).toInt, (heightBloc+1).toInt))
  }

  def drawCell(g: Graphics2D, x: Double, y: Double, cell: Cell, openDown: Boolean, widthBlock: Double, heightBlock: Double) {
  	var i = 0
  	g.fill(new Rectangle(x.toInt, y.toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
    g.setColor(Color.BLACK)
  	if(!cell.openNorth) g.fill(new Rectangle((x + widthBlock).toInt, y.toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
  	if(!cell.openWest) g.fill(new Rectangle(x.toInt, (y + heightBlock).toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
  	if(cell.marked) {
  		g.setColor(Color.GREEN)
      g.fill(new Rectangle((x + widthBlock).toInt, (y + heightBlock).toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
    }
    if(cell.markedTest) {
      g.setColor(Color.RED)
      g.fill(new Rectangle((x + widthBlock).toInt, (y + heightBlock).toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
    }


    if(cell.openUp || openDown) {
      if(cell.openUp && openDown) g.setColor(Color.GREEN)
      else if(cell.openUp) g.setColor(Color.YELLOW)
      else if(openDown) g.setColor(Color.BLUE)
    	g.draw(new Rectangle((x + widthBlock).toInt, (y + heightBlock).toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
    }
  	g.setColor(Color.BLACK)


  }
}