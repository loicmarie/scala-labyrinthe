
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
	
	def findDeadEnds() {
		var x = 0
		var y = 0
		var z = 0
		var num = 0
		for(z <- 0 to maze.length-1){
			for(y <- 0 to maze(z).length-1){
				for(x <- 0 to maze(z)(y).length-1){
					if(isDeadEnd(x,y,z)) {
						maze(z)(y)(x).markTest
						fillDeadEnd(x, y, z, num)
					}
				}
			}
		}
	}

	def isDeadEnd(x: Int, y: Int, z: Int): Boolean = {
		var count = 0
		if(!openNorth(x,y,z)) count = count + 1
		if(!openWest(x,y,z)) count = count + 1
		if(!openEst(x,y,z)) count = count + 1
		if(!openSouth(x,y,z)) count = count + 1
		if(!openDown(x,y,z)) count = count + 1
		if(!openUp(x,y,z)) count = count + 1
		return (count == 5)
	}

	def fillDeadEnd(x: Int, y: Int, z: Int, num:Int) {
		var _num = num+1
			maze(z)(y)(x).mark
		if((!isGoal(x, y, z))){
			if(openNorth(x,y,z)) {
				setCloseNorth(x,y,z)
			} else if(openWest(x,y,z)) {
				setCloseWest(x,y,z)
			} else if(openEst(x,y,z)) {
				setCloseEst(x,y,z)
			} else if(openSouth(x,y,z)) {
				setCloseSouth(x,y,z)
			} else if(openDown(x,y,z)) {
				setCloseDown(x,y,z)
			} else if(openUp(x,y,z)) {
				setCloseUp(x,y,z)
			}
			if(x-1 > 0 && isDeadEnd(x-1, y, z)) fillDeadEnd(x-1, y, z, _num)
			if(y-1 > 0 && isDeadEnd(x, y-1, z)) fillDeadEnd(x, y-1, z, _num)
			if(z-1 > 0 && isDeadEnd(x, y, z-1)) fillDeadEnd(x, y, z-1, _num)
			if(x < maze(maze.length-1)(maze(maze.length-1).length-1).length-1 && isDeadEnd(x+1, y, z)) fillDeadEnd(x+1, y, z, _num)
			if(y < maze(maze.length-1).length-1 && isDeadEnd(x, y+1, z)) fillDeadEnd(x, y+1, z, _num)
			if(z < maze.length-1 && isDeadEnd(x, y, z+1)) fillDeadEnd(x, y, z+1, _num)
			findDeadEnds()
		}
	}
	
	def openNorth(x: Int, y: Int, z: Int): Boolean = 
		maze(z)(y)(x).openNorth
	
	def openWest(x: Int, y: Int, z: Int): Boolean = 
		maze(z)(y)(x).openWest
	
	def openUp(x: Int, y: Int, z: Int): Boolean = 
		maze(z)(y)(x).openUp
	
	def openDown(x: Int, y: Int, z: Int): Boolean = {
		if(z < maze.length-1) maze(z+1)(y)(x).openUp
		else    			false
	}
	
	def openEst(x: Int, y: Int, z: Int): Boolean = {
		if(x < maze(z)(y).length-1) maze(z)(y)(x+1).openWest
		else                      false
	}
	
	def openSouth(x: Int, y: Int, z: Int): Boolean = {
		if(y < maze(z).length-1) maze(z)(y+1)(x).openNorth
		else    			   false
	}
	
	def setCloseNorth(x: Int, y: Int, z: Int) = 
		maze(z)(y)(x).setCloseNorth
	
	def setCloseWest(x: Int, y: Int, z: Int) = 
		maze(z)(y)(x).setCloseWest
	
	def setCloseUp(x: Int, y: Int, z: Int) = 
		maze(z)(y)(x).setCloseUp
	
	def setCloseDown(x: Int, y: Int, z: Int) = {
		if(z > maze.length-1) maze(z+1)(y)(x).setCloseUp
	}
	
	def setCloseEst(x: Int, y: Int, z: Int) = {
		if(x < maze(z)(y).length-1) maze(z)(y)(x+1).setCloseWest
	}
	
	def setCloseSouth(x: Int, y: Int, z: Int) = {
		if(y < maze(z).length-1) maze(z)(y+1)(x).setCloseNorth
	}

	def isGoal(x: Int, y: Int, z: Int): Boolean = {
		return (x==19
			&& y==19
			&& z==0)
	}

	def isStart(x: Int, y: Int, z: Int): Boolean = {
		return (x==0 && y==0 && z==0)
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