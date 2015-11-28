import scala.util.Random
 
object MazeTypes {
  case class Direction(val dx: Int, val dy: Int, val dz: Int)
 
  case class Loc(val x: Int, val y: Int, val z: Int) {
    def +(that: Direction): Loc = Loc(x + that.dx, y + that.dy, z + that.dz)
  }
 
  case class Door(val from: Loc, to: Loc)
 
  val North = Direction(0,-1,0)
  val South = Direction(0,1,0)
  val West = Direction(-1,0,0)
  val East = Direction(1,0,0)
  val Up = Direction(0,0,1)
  val Down = Direction(0,0,-1)
  val directions = Set(North, South, West, East, Up, Down)
}
 
import MazeTypes._

object Maze3D {
 
  def shuffle[T](set: Set[T]): List[T] = Random.shuffle(set.toList)
 
  def buildImpl(current: Loc, grid: Grid): Grid = {
    var newgrid = grid.markVisited(current)
    val nbors = shuffle(grid.neighbors(current))
    nbors.foreach { n =>
      if (!newgrid.isVisited(n)) {
        newgrid = buildImpl(n, newgrid.markVisited(current).addDoor(Door(current, n)))
      }
    }
    newgrid
  }
 
  def build(width: Int, height: Int, depth: Int): Grid = {
    val exit = Loc(width-1, height-1, depth-1)
    val grid = buildImpl(exit, new Grid(width, height, depth, Set(), Set()))
	//grid.printMaze()
	print(grid.get)
	grid
  }  
  
  def main(args: Array[String]) {
    build(8,8,3)
  }
  
}

class Cell() {
  var openNorth = false
  var openWest = false
  var openUp = false

  def setOpenNorth {
    openNorth = true
  }
  def setOpenWest {
    openWest = true
  }
  def setOpenUp {
    openUp = true
  }
}

class Grid(val width: Int, val height: Int, val depth: Int, val doors: Set[Door], val visited: Set[Loc]) {

  def addDoor(door: Door): Grid = 
    new Grid(width, height, depth, doors + door, visited)
 
  def markVisited(loc: Loc): Grid = 
    new Grid(width, height, depth, doors, visited + loc)
 
  def isVisited(loc: Loc): Boolean = 
    visited.contains(loc)
 
  def neighbors(current: Loc): Set[Loc] = 
    directions.map(current + _).filter(inBounds(_)) -- visited
 
  def get(): List[List[List[Cell]]] = {
    var maze = List[List[List[Cell]]]()
    var x = 0
    var y = 0
    var z = 0
    var current = new Loc(0,0,0)
    for(z <- 0 to depth-1){
		print(x)
      for(y <- 0 to height-1){
		print(y)
		for(x <- 0 to width-1){
		print(z)
		  print(maze(z)(y)(x))
          current = new Loc(x,y,z)
          if(openUp(current)) maze(z)(y)(x).setOpenUp
          if(openWest(current)) maze(z)(y)(x).setOpenWest
          if(openNorth(current)) maze(z)(y)(x).setOpenNorth
        }
      }
    }
	print(maze)
    maze
  }

  def printMaze(){
    var z = 0
    for(z <- 0 to depth-1){
      printGrid(z)
    }
  }

  def printGrid(z: Int) {
    //(0 to height).toList.flatMap(y => printRow(y))
  	var j = 0
  	for(j <- 0 to width-1){
  		printRow(z, j)
  		println("")
  	}
  	for(j <- 0 to width-1){
  		print("[][][]")
  	}
  	print("[]")
    j = 0
    println("")
    println("")
  }
 
  private def inBounds(loc: Loc): Boolean = 
    loc.x >= 0 && loc.x < width && loc.y >= 0 && loc.y < height && loc.z >= 0 && loc.z < depth
 
  def printRow(z: Int, y: Int) = {
    val row = (0 until width).toList.map(x => printCell(Loc(x, y, z)))
    val rightSide = if (y == height-1) "  " else "[]"
    val newRow = row :+ List("[]", rightSide)
  	var top = List[String]()
  	var left = List[String]()
  	newRow.zipWithIndex.foreach{ case(x,i) =>
  		top = top :+ x(0)
  		left = left :+ x(1)
  	}
  	top.zipWithIndex.foreach{ case(x,i) =>
  		print(x)
  	}
  	println("")
  	left.zipWithIndex.foreach{ case(x,i) =>
  		print(x)
  	}
  }
 
  private val entrance = Loc(0,0,0)
 
  def printCell(loc: Loc): List[String] = {
	var midVal = "    "
	if(openUp(loc)) midVal = "XX  " 
    if (loc.y == height) 
      List("[][][]")
    else List(
      if (openNorth(loc)) "[]  []" else "[][][]", 
      if (openWest(loc) || loc == entrance) "  ".concat(midVal) else "[]".concat(midVal)
    )
  }
 
  def openNorth(loc: Loc): Boolean = openInDirection(loc, North)
 
  def openWest(loc: Loc): Boolean = openInDirection(loc, West)

  def openUp(loc: Loc): Boolean = openInDirection(loc, Up)
 
  private def openInDirection(loc: Loc, dir: Direction): Boolean = 
    doors.contains(Door(loc, loc + dir)) || doors.contains(Door(loc + dir, loc))
}