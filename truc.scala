class Node(id : Int) {
  var edges = List[Edge]()

  def addEdge(other : Node, weight : Int) = {
    val edge = new Edge(this, other, weight)
    edges = edge :: edges
    edge
  }

  override def toString() = id.toString
}

class Edge(val from : Node, val to : Node, val weight : Int) extends Ordered[Edge] {
  // Inverse ordering; should really be external.
  def compare(that : Edge) = that.weight compare weight

  override def toString() = from + " <--> " + to + "    (" + weight + ")"
}

object Main {

  def main(args : Array[String]) {
    val n1 = new Node(1)
    val n2 = new Node(2)
    val n3 = new Node(3) 
    val n4 = new Node(4)
    val n5 = new Node(5)
    val n6 = new Node(6)

    n1.addEdge(n2, 6)
    n1.addEdge(n3, 1)
    n1.addEdge(n4, 5)
    n2.addEdge(n1, 6)
    n2.addEdge(n3, 5)
    n2.addEdge(n5, 3)

    n3.addEdge(n1, 1)
    n3.addEdge(n2, 5)
    n3.addEdge(n4, 5)
    n3.addEdge(n5, 6)
    n3.addEdge(n6, 4)

    n4.addEdge(n1, 5)
    n4.addEdge(n3, 5)
    n4.addEdge(n6, 2)

    n5.addEdge(n2, 3)
    n5.addEdge(n3, 6)
    n5.addEdge(n6, 6)

    n6.addEdge(n3, 4)
    n6.addEdge(n4, 2)
    n6.addEdge(n5, 6)

    val graph = List(n1, n2, n3, n4, n5, n6)

    generateMST(graph).sortWith(_ < _).foreach(println)
  }

  // Prim's algorithm:
  //   * Choose a random node.
  //   * Place edges from that node in priority queue.
  //   * While queue not empty:
  //   *   pop highest weighted edge
  //   *   if end-point in already-included set, continue
  //   *   else:
  //   *     add edge to edge-set
  //   *     add node to node-set
  //   *     add edges from new node to queue

  def generateMST(graph : List[Node]) : List[Edge] = {
    import scala.util.Random

    val startNode = graph(Random.nextInt(graph.length))

    var mst_nodes = List(startNode)
    var mst_edges = List[Edge]()

    val pq = new scala.collection.mutable.PriorityQueue[Edge]

    startNode.edges.foreach(e => pq.enqueue(e))

    while (!pq.isEmpty) {
      val edge = pq.dequeue

      if (!(mst_nodes contains edge.to)) {
        mst_nodes = edge.to :: mst_nodes
        mst_edges = edge :: mst_edges

        edge.to.edges.foreach(e => pq.enqueue(e))
      }
    }

    mst_edges
  }
}