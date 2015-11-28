class MazeSolver(_maze: Array[Array[Array[Cell]]]) {
	
	var maze = _maze
	
	def findPath(Int: x, Int: y, Int: z): Boolean = {
		
	}
	def mark(x: Int, y: Int, z: Int) = {
		maze(z)(y)(x).mark()
	}
	def unmark(x: Int, y: Int, z: Int) = {
		maze(z)(y)(x).unmark()
	}
	def isGoal(x: Int, y: Int, z: Int): Boolean = {
		return (x==0 && y==0 && z==0)		
	}
	def canAccess(): ={
		
	}
	def inBounds(): ={
	}
}




var maze = new Maze3D()
var truc = new MazeSolver(maze)
truc.findPath(0,0,0)


for(z <- 0 to maze-1){
  for(y <- 0 to maze(z)-1){
	for(x <- 0 to maze(z)(y)-1){
		if(maze(z)(y)(x).openUp)
	}
  }
}