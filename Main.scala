object HelloWorld {
  def main(args: Array[String]) {
    val maze = new Maze(39, 39)
	val labyrinth = maze.get
	maze.show
  }
}