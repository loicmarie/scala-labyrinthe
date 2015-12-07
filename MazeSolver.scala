
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
class DeadEndFiller(_maze: Array[Array[Array[Cell]]], _mazeCopy: Array[Array[Array[Cell]]]) {
	
	var maze = _maze
	var mazeCopy = _mazeCopy
	
	def init() {
		var x = 0
		var y = 0
		var z = 0
		var num = 0
		for(z <- 0 to maze.length-1){
			for(y <- 0 to maze(z).length-1){
				for(x <- 0 to maze(z)(y).length-1){
					if(isDeadEnd(x,y,z)) {
						// maze(z)(y)(x).markTest
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
		maze(z)(y)(x).markTest

		if((!isGoal(x, y, z))){

			if(openNorth(x,y,z)) setCloseNorth(x,y,z)
			else if(openWest(x,y,z)) setCloseWest(x,y,z)
			else if(openEst(x,y,z)) setCloseEst(x,y,z)
			else if(openSouth(x,y,z)) setCloseSouth(x,y,z)
			else if(openDown(x,y,z)) setCloseDown(x,y,z)
			else if(openUp(x,y,z)) setCloseUp(x,y,z)
			
			if(x-1 > 0 && isDeadEnd(x-1, y, z)) fillDeadEnd(x-1, y, z, _num)
			if(y-1 > 0 && isDeadEnd(x, y-1, z)) fillDeadEnd(x, y-1, z, _num)
			if(z-1 > 0 && isDeadEnd(x, y, z-1)) fillDeadEnd(x, y, z-1, _num)
			if(x < maze(maze.length-1)(maze(maze.length-1).length-1).length-1 && isDeadEnd(x+1, y, z)) fillDeadEnd(x+1, y, z, _num)
			if(y < maze(maze.length-1).length-1 && isDeadEnd(x, y+1, z)) fillDeadEnd(x, y+1, z, _num)
			if(z < maze.length-1 && isDeadEnd(x, y, z+1)) fillDeadEnd(x, y, z+1, _num)

			if(countDeadEnds > 0) init()
		}
	}

	def countDeadEnds(): Int = {
	    var x = 0
	    var y = 0
	    var z = 0
	    var num = 0
	    for(z <- 0 to maze.length-1){
	      for(y <- 0 to maze(z).length-1){
	        for(x <- 0 to maze(z)(y).length-1){
	          if(isDeadEnd(x,y,z)) num = num+1
	        }
	      }
	    }
	    num
	}

	def getSolution: Array[Array[Array[Cell]]] = {
	    var x = 0
	    var y = 0
	    var z = 0
	    var num = 0
	    for(z <- 0 to maze.length-1){
	      for(y <- 0 to maze(z).length-1){
	        for(x <- 0 to maze(z)(y).length-1){
	          if(!maze(z)(y)(x).markedTest) mazeCopy(z)(y)(x).mark
	          else mazeCopy(z)(y)(x).unmark
	          if(isGoal(x,y,z)) mazeCopy(z)(y)(x).mark
	        }
	      }
	    }
	    mazeCopy
	}
	
	def openNorth(x: Int, y: Int, z: Int): Boolean = 
		maze(z)(y)(x).openNorth
	
	def openWest(x: Int, y: Int, z: Int): Boolean = 
		maze(z)(y)(x).openWest
	
	def openUp(x: Int, y: Int, z: Int): Boolean = 
		maze(z)(y)(x).openUp
	
	def openDown(x: Int, y: Int, z: Int): Boolean = {
		if(z > 0) maze(z-1)(y)(x).openUp
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
		if(z > 0) maze(z-1)(y)(x).setCloseUp
	}
	
	def setCloseEst(x: Int, y: Int, z: Int) = {
		if(x < maze(z)(y).length-1) maze(z)(y)(x+1).setCloseWest
	}
	
	def setCloseSouth(x: Int, y: Int, z: Int) = {
		if(y < maze(z).length-1) maze(z)(y+1)(x).setCloseNorth
	}

	def isGoal(x: Int, y: Int, z: Int): Boolean = {
		return (x==maze(z)(y).length-1
			&& y==maze(z).length-1
			&& z==maze.length-1)
	}

	def isStart(x: Int, y: Int, z: Int): Boolean = {
		return (x==0 && y==0 && z==0)
	}
}

class RecursiveBacktracking(_maze: Array[Array[Array[Cell]]], _mazeCopy: Array[Array[Array[Cell]]]) {
	
	var maze = _maze
	var mazeCopy = _mazeCopy

	def init(){
		findPath(-1,0,0,0,0,0)
	}

	def findPath(x1: Int, y1: Int, z1: Int, x: Int, y: Int, z: Int): Boolean = {
		
		var inBounds = (z>=0 && z < maze.length
			&& y>=0 && y < maze(0).length
			&& x>=0 && x < maze(0)(0).length)

		if(inBounds && isGoal(x, y, z)) {
			mark(x, y, z)
			return true
		}

		if(canAccess( x1, y1, z1, x, y, z, x, y, z+1) && findPath(x, y, z, x, y, z+1)) {
			mark(x, y, z)
			return true
		} else if(canAccess( x1, y1, z1, x, y, z, x-1, y, z) && findPath(x, y, z, x-1, y, z)) {
			mark(x, y, z)
			return true
		} else if(canAccess( x1, y1, z1, x, y, z, x, y-1, z) && findPath(x, y, z, x, y-1, z)) {
			mark(x, y, z)
			return true
		} else if(canAccess( x1, y1, z1, x, y, z, x, y, z-1) && findPath(x, y, z, x, y, z-1)) {
			mark(x, y, z)
			return true
		} else if(canAccess( x1, y1, z1, x, y, z, x, y+1, z) && findPath(x, y, z, x, y+1, z)) {
			mark(x, y, z)
			return true
		} else if(canAccess( x1, y1, z1, x, y, z, x+1, y, z) && findPath(x, y, z, x+1, y, z)) {
			mark(x, y, z)
			return true
		} else return false
		
	}
	
	def isGoal(x: Int, y: Int, z: Int): Boolean = {
		return (x==maze(0)(0).length-1 && y==maze(0).length-1 && z==maze.length-1)
	}

	def openNorth(x: Int, y: Int, z: Int): Boolean = 
		return maze(z)(y)(x).openNorth
	
	def openWest(x: Int, y: Int, z: Int): Boolean = 
		return maze(z)(y)(x).openWest
	
	def openUp(x: Int, y: Int, z: Int): Boolean = 
		return maze(z)(y)(x).openUp
	
	def openDown(x: Int, y: Int, z: Int): Boolean = {
		if(z > 0) return maze(z-1)(y)(x).openUp
		else    			return false
	}
	
	def openEst(x: Int, y: Int, z: Int): Boolean = {
		if(x < maze(z)(y).length-1) return maze(z)(y)(x+1).openWest
		else                      return false
	}
	
	def openSouth(x: Int, y: Int, z: Int): Boolean = {
		// if(y < maze(z).length-1 && isGoal(x,y+1,z)) println(maze(z)(y+1)(x).openNorth)
		if(y < maze(z).length-1) return maze(z)(y+1)(x).openNorth
		else    			   return false
	}

	def mark(x: Int, y: Int, z: Int) = {
		maze(z)(y)(x).mark
	}

	def unmark(x: Int, y: Int, z: Int) = {
		maze(z)(y)(x).unmark
	}

	def canAccess(x: Int, y: Int, z:Int, x1: Int, y1: Int, z1:Int, x2: Int, y2: Int, z2:Int): Boolean = {

		var inBoundsDest = (z2>=0 && z2 < maze.length
			&& y2>=0 && y2 < maze(0).length
			&& x2>=0 && x2 < maze(0)(0).length)

		if (math.sqrt((x1-x)*(x1-x)+(y1-y)*(y1-y)+(z1-z)*(z1-z))==1 && inBoundsDest){
				if(x2-x1==1 && x2-x!=0) return openEst(x1,y1,z1)
				if(x2-x1==(-1) && x2-x!=0) return openWest(x1,y1,z1)
				if(y2-y1==1 && y2-y!=0) return openSouth(x1,y1,z1)
				if(y2-y1==(-1) && y2-y!=0) return openNorth(x1,y1,z1)
				if(z2-z1==1 && z2-z!=0) return openUp(x1,y1,z1)
				if(z2-z1==(-1) && z2-z!=0) return openDown(x1,y1,z1)
		}
		return false
	}

	def getSolution: Array[Array[Array[Cell]]] = {
	    var x = 0
	    var y = 0
	    var z = 0
	    var num = 0
	    for(z <- 0 to maze.length-1){
	      for(y <- 0 to maze(z).length-1){
	        for(x <- 0 to maze(z)(y).length-1){
	          if(maze(z)(y)(x).marked) mazeCopy(z)(y)(x).mark
	          else mazeCopy(z)(y)(x).unmark
	        }
	      }
	    }
	    mazeCopy
	}
}