import java.awt.{Color, Graphics2D, Dimension, Stroke, BasicStroke}
import javax.swing.JFileChooser
import java.io.File
import scala.swing._
import scala.swing.event._

object SecondSwingApp extends SimpleSwingApplication {
  def top = new MainFrame {

    val generator = new Maze3D(30, 30, 5)
    var maze = generator.build

    var depth = 0
    maze(0)(0)(0).setOpenWest
    var mazeCopy = copyMaze(maze)

  var widthApp  = 1100.0
  var heightApp = 800.0

  var widthCell = widthApp/(maze(0)(0).length)
  var heightCell = heightApp/(maze(0).length)

  var widthBloc = widthCell/2
  var heightBloc = heightCell/2

  var widthScreen  = widthApp + widthBloc
  var heightScreen = heightApp + heightBloc

    var time = 0

    var panel = new DataPanel(maze, widthCell, heightCell, widthBloc, heightBloc, depth) {
      preferredSize = new Dimension(widthScreen.toInt, heightScreen.toInt)
    }

    val button2 = Button("Recursive Backtracking")(recursiveBacktracking())
    val button3 = Button("Dead End Filler")(deadEndFiller())
    val button4 = Button("Reset")(reset())
    val button5 = Button("-1")(changeDepth(-1))
    val button6 = Button("+1")(changeDepth(1))
    val button7 = Button("Import file")(run())
    val button = Button("Maze generation")(generate())
    var showTime = new Label {
      text = "Time: " + (time/1000) + " µs"
    }

    def run() {
        var chooser = new JFileChooser();
        chooser.setCurrentDirectory(new java.io.File("."));
        chooser.setDialogTitle("Choose maze file");
        // chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        // chooser.setAcceptAllFileFilterUsed(false);
        if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
          System.out.println("getCurrentDirectory(): " + chooser.getCurrentDirectory());
          System.out.println("getSelectedFile() : " + chooser.getSelectedFile());
          readMaze("" + chooser.getSelectedFile())
        } else {
          System.out.println("No Selection ");
        }
     }

    updateView()

    def generate() {
      maze = generator.build
      maze(0)(0)(0).setOpenWest
      mazeCopy = copyMaze(maze)
      updateView()
    }

    def deadEndFiller() {
      reset
      time = System.nanoTime.toInt
      var mazeCopy1 = copyMaze(mazeCopy)
      var solver1 = new DeadEndFiller(copyMaze(mazeCopy), mazeCopy1)
      solver1.init
      maze = solver1.getSolution
      time = System.nanoTime.toInt - time
      updateView()
    }

    def recursiveBacktracking() {
      reset
      time = System.nanoTime.toInt
      var mazeCopy2 = copyMaze(mazeCopy)
      var solver2 = new RecursiveBacktracking(copyMaze(mazeCopy), mazeCopy2)
      solver2.init
      maze = solver2.getSolution
      time = System.nanoTime.toInt - time
      updateView()
    }

    def updateView(){

      widthCell = widthApp/(maze(0)(0).length)
      heightCell = heightApp/(maze(0).length)

      widthBloc = widthCell/2
      heightBloc = heightCell/2

      widthScreen  = widthApp + widthBloc
      heightScreen = heightApp + heightBloc
      panel = new DataPanel(maze, widthCell, heightCell, widthBloc, heightBloc, depth) {
        preferredSize = new Dimension(widthScreen.toInt, heightScreen.toInt)
      }
      showTime = new Label {
        text = "Time: " + (time/1000) + " µs"
      }
      contents = new BoxPanel(Orientation.Vertical) {
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += button
          contents += button2
          contents += button3
          contents += showTime
          contents += button4
          contents += button5
          contents += button6
          contents += button7
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


    def readMaze(filePath : String){
      println("Read maze from file : "+filePath);

      var WIDTH = 0
      var HEIGHT = 0
      var DEPTH = 0
      var cells = Array.ofDim[Int](0, 0, 0)

      // Get all lines from the file
      val lines = io.Source.fromFile(filePath).getLines

      var indexLvl = 0
      var indexLine = 0
      var indexChar = 0

      // for each line of the file
      lines.foreach(line => {
        // The first line of the file (init dimensions)
        if(indexLine == 0 && indexLvl == 0){
          // set the width and height and depth
          val dim = line.split(" ");
          if(dim.length >= 2) {
            WIDTH = dim(0).toInt;
            HEIGHT = dim(1).toInt;
            if(dim.length == 3)
              DEPTH = dim(2).toInt;
            else
              DEPTH = 1
            cells = Array.tabulate(DEPTH)(i => Array.tabulate(HEIGHT)(j => Array.tabulate(WIDTH)(k => 0)))
          } else {
            println("Bad file (dimensions)")
            return
          }
        } else {
          // empty line 
          if(line.length == 0) {
            // start new maze lvl
            indexLine = 0;
            indexLvl += 1;
          } else {
            // Read the line
            indexChar = 0;
            val charList = line.toList

            // for each char of the line
            // var chars = new Array[Int](WIDTH)
            // var count = 0
            charList.foreach(char => {
              // chars(count) = char
              if(char.toInt == 49) {// It is a wall
                cells(indexLvl)(indexLine-1)(indexChar) = 1;
              } else if(char.toInt == 50) {// can go up from this cell
                cells(indexLvl)(indexLine-1)(indexChar) = 2;
              } else if(char.toInt == 51) {// can go down from this cell
                cells(indexLvl)(indexLine-1)(indexChar) = 3;
              } else if(char.toInt == 52) {// can go up and down from this cell
                cells(indexLvl)(indexLine-1)(indexChar) = 4;
              } else {
                cells(indexLvl)(indexLine-1)(indexChar) = 0;
              }
              indexChar+= 1
            })  
            // floor(countFloors) = chars                
          }
        }

        indexLine+=1
      });

      var y = 0
      var z = 0
      var num = 0
      var xMax = (cells(0)(0).length-2)/2
      var yMax = (cells(0).length-2)/2
      var res = Array.ofDim[Cell](cells.length, cells(0).length-2, cells(0)(0).length-2)
      var north = 0
      var west  = 0
      var xCurrent  = 0
      var yCurrent  = 0
      // if(cells(0).length%2==1) yMax = (cells(0).length-1)/2
      // else 
      // yMax = 
      // if(cells(0)(0).length%2==1) xMax = (cells(0)(0).length-1)/2
      // else 
      // xMax = 
      // for(z <- 0 to cells.length-1){
      //   for(y <- 1 to 1 by -1){
      //     north = y*2 - 1
      //     yCurrent = y*2
      //     for(x <- xMax to 1 by -1){
      //       west = x*2 - 1
      //       xCurrent = x*2
      //       res(z)(y-1)(x-1) = new Cell()
      //       if(cells(z)(yCurrent)(xCurrent) == 2 || cells(z)(yCurrent)(xCurrent) == 4){
      //         res(z)(y-1)(x-1).setOpenUp
      //       } 
      //       if(cells(z)(north)(xCurrent) == 0 && (cells(z)(yCurrent)(xCurrent) == 0 || cells(z)(yCurrent)(xCurrent) == 2 || cells(z)(yCurrent)(xCurrent) == 4)){
      //         res(z)(y-1)(x-1).setOpenNorth
      //       } 
      //       if(cells(z)(yCurrent)(west) == 0 && (cells(z)(yCurrent)(xCurrent) == 0 || cells(z)(yCurrent)(xCurrent) == 2 || cells(z)(yCurrent)(xCurrent) == 4)){
      //         res(z)(y-1)(x-1).setOpenWest
      //       }
      //     }
      //     println("")
      //   }
      //   println("")
      //   println("")

      // }


      for(z <- 0 to cells.length-1){
        for(y <- 1 to cells(0).length-2){
          north = y*2 - 1
          yCurrent = y*2
          for(x <- 1 to cells(0)(0).length-2){
            res(z)(y-1)(x-1) = new Cell()
            if(cells(z)(y)(x) == 2 || cells(z)(y)(x) == 4){
              res(z)(y-1)(x-1).setOpenUp
            } 
            if(y-1 > 0 && cells(z)(y-1)(x) == 0 && (cells(z)(y)(x) == 0 || cells(z)(y)(x) == 2 || cells(z)(y)(x) == 4)){
              res(z)(y-1)(x-1).setOpenNorth
            } 
            if(x-1 > 0 && cells(z)(y)(x-1) == 0 && (cells(z)(y)(x) == 0 || cells(z)(y)(x) == 2 || cells(z)(y)(x) == 4)){
              res(z)(y-1)(x-1).setOpenWest
            }
          }
        }
      }
      res(0)(0)(0).setOpenWest
      res(0)(0)(0).mark
      maze = res
      mazeCopy = copyMaze(maze)
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
      var isNorthMarked = false
      var isWestMarked = false
      g.setColor(Color.BLACK)
      for(j <- 0 to floor.length-1){
      	x = 0.0
      	for(i <- 0 to floor(j).length-1){
	      	if(i == floor(j).length-1) g.fill(new Rectangle((x + widthCell).toInt, y.toInt, (widthBloc+1).toInt, (heightCell+1).toInt))
	      	if(j == floor.length-1)    g.fill(new Rectangle(x.toInt, (y + heightCell).toInt, (widthCell+1).toInt, (heightBloc+1).toInt))
      		if(currentDepth > 0) openDown = maze(currentDepth-1)(j)(i).openUp
          else openDown = false
          if(j>0 && maze(currentDepth)(j-1)(i).marked) isNorthMarked = true
          if(i>0 && maze(currentDepth)(j)(i-1).marked) isWestMarked = true
          drawCell(g, x, y, floor(j)(i), openDown, widthBloc, heightBloc, isNorthMarked, isWestMarked)
      		x = x + widthCell
      	}
      	y = y + heightCell
      }
	  g.fill(new Rectangle(x.toInt, y.toInt, (widthBloc+1).toInt, (heightBloc+1).toInt))
  }

  def drawCell(g: Graphics2D, x: Double, y: Double, cell: Cell, openDown: Boolean, widthBlock: Double, heightBlock: Double, isNorthMarked: Boolean, isWestMarked: Boolean) {
  	
    var thickness = 4;
    g.setStroke(new BasicStroke(thickness));

    var i = 0
  	g.fill(new Rectangle(x.toInt, y.toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
    g.setColor(Color.BLACK)
  	if(!cell.openNorth) g.fill(new Rectangle((x + widthBlock).toInt, y.toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
    else if(cell.marked && isNorthMarked) {
      g.setColor(Color.GREEN)
      g.fill(new Rectangle((x + widthBlock).toInt, y.toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
      g.setColor(Color.BLACK)
    }
    if(!cell.openWest) g.fill(new Rectangle(x.toInt, (y + heightBlock).toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
    else if(cell.marked && isWestMarked) {
      g.setColor(Color.GREEN)
      g.fill(new Rectangle(x.toInt, (y + heightBlock).toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
      g.setColor(Color.BLACK)
    }
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
      else if(cell.openUp) g.setColor(Color.RED)
      else if(openDown) g.setColor(Color.BLUE)
    	g.draw(new Rectangle((x + widthBlock).toInt, (y + heightBlock).toInt, (widthBlock+1).toInt, (heightBlock+1).toInt))
    }
  	g.setColor(Color.BLACK)


  }
}