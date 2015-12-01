import java.awt.{Color, Graphics2D, Dimension}
import scala.swing._

object HelloWorld {
  def main(args: Array[String]) {
    val generator = new Maze3D(15, 15, 3)
    val maze1 = generator.build
  }
}

object SecondSwingApp extends SimpleSwingApplication {
  def top = new MainFrame {

    val generator = new Maze3D(20, 20, 3)
    val maze = generator.build

	var widthApp  = 700
	var heightApp = 700

	var widthCell = widthApp/(maze(0)(0).length+1) + 1
	var heightCell = heightApp/(maze(0).length+1) + 1

	var widthBloc = widthCell/2
	var heightBloc = heightCell/2

	var widthScreen  = widthApp - 2*widthBloc
	var heightScreen = heightApp -2*heightBloc

    contents = new DataPanel(maze, widthCell, heightCell, widthBloc, heightBloc) {
      preferredSize = new Dimension(widthScreen, heightScreen)
    }
  }
}  

class DataPanel(maze: Array[Array[Array[Cell]]], widthCell: Int, heightCell: Int, widthBloc: Int, heightBloc: Int) extends Panel {

  override def paintComponent(g: Graphics2D) {
    /*val dx = g.getClipBounds.width.toFloat  / data.length
    val dy = g.getClipBounds.height.toFloat / data.map(_.length).max
    for {
      x <- 0 until data.length
      y <- 0 until data(x).length
      x1 = (x * dx).toInt
      y1 = (y * dy).toInt
      x2 = ((x + 1) * dx).toInt
      y2 = ((y + 1) * dy).toInt
    } {
      data(x)(y) match {
        case c: Color => g.setColor(c)
        case _ => g.setColor(Color.WHITE)
      }*/
      maze(0)(2)(3).mark
      maze(0)(3)(2).mark
      maze(0)(4)(5).mark
      drawFloor(g, maze(0), widthCell, heightCell, widthBloc, heightBloc)
    //}
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
  }
}