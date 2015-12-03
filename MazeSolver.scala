/*

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
	def canAccess(x: Int, y: Int, z:Int, x1: Int, y1: Int, z1:Int): Boolean = {
		
		
		
		if (x==0 && y==0 && z== 0){
		return true
		}
		else if (x==0 && y==0 && z== 1){
		return true
		}
		else if (x==0 && y==1 && z== 0){
		return true
		}
		else if (x==1 && y==0 && z== 0){
		return true
		}
		else if (x==0 && y==0 && z== 0){
		return true
		}
		else if (x==0 && y==0 && z== 0){
		return true
		}
		else if (x==0 && y==0 && z== 0){
		return true
		}
	}

}
*/
class DeadEndFiller(_maze: Array[Array[Array[Cell]]]) {
	
	var maze = _maze
	
	def fillDeadEnd() {
		var x = 0
		var y = 0
		var z = 0
		var count = 0
		for(z <- 0 to maze.length-1){
			for(y <- 0 to maze(z).length-1){
				for(x <- 0 to maze(z)(y).length-1){
					count = 0
					if(!openNorth(x,y,z,maze)) count = count + 1
					if(!openWest(x,y,z,maze)) count = count + 1
					if(!openEst(x,y,z,maze)) count = count + 1
					if(!openSouth(x,y,z,maze)) count = count + 1
					if(count == 3){
						maze(z)(y)(x).mark
					}
				}
			}
		}
	}
	
	def openNorth(x: Int, y: Int, z: Int, maze: Array[Array[Array[Cell]]]): Boolean = 
		maze(z)(y)(x).openNorth
	
	def openWest(x: Int, y: Int, z: Int, maze: Array[Array[Array[Cell]]]): Boolean = 
		maze(z)(y)(x).openWest
	
	def openUp(x: Int, y: Int, z: Int, maze: Array[Array[Array[Cell]]]): Boolean = 
		maze(z)(y)(x).openUp
	
	def openDown(x: Int, y: Int, z: Int, maze: Array[Array[Array[Cell]]]): Boolean = {
		if(z>1) maze(z-1)(y)(x).openUp
		else    false
	}
	
	def openEst(x: Int, y: Int, z: Int, maze: Array[Array[Array[Cell]]]): Boolean = {
		if(x<maze(z)(y).length-2) maze(z)(y)(x+1).openWest
		else                      false
	}
	
	def openSouth(x: Int, y: Int, z: Int, maze: Array[Array[Array[Cell]]]): Boolean = {
		if(y<maze(z).length-2){
			if(maze(z)(y+1)(x).openNorth){
				maze(z)(y+1)(x).markTest
			}
			maze(z)(y+1)(x).openNorth
		}else    false
	}
}

/*
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
*/