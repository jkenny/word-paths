import annotation.tailrec
import collection.mutable.{HashSet, Set}
import scala.io.Source

object Words {

  private val ValidWordLength = 4

  private lazy val dictionary: Map[String, Iterable[String]] = loadDictionary

  private def loadDictionary: Map[String, Iterable[String]] = {

    var startTime = System.currentTimeMillis()

    val dict = Source.fromFile("/usr/share/dict/words")
    val words = dict.getLines().filter(_.length == ValidWordLength).map(_.toLowerCase).toIterable

    println("Dictionary (" + words.size + " words) loaded in: " + (System.currentTimeMillis() - startTime).abs + "ms")

    startTime = System.currentTimeMillis()

    val resultMap = words.par.map {
      word =>
        word -> words.filter(areNeighbours(word, _)).seq.toIterable
    }.seq.toMap

    println("Map (" + resultMap.size + " words) built in: " + (System.currentTimeMillis() - startTime).abs + "ms")

    dict.close()

    resultMap
  }

  def areNeighbours(word1: String, word2: String): Boolean = {
    var difference = 0
    (0 until word1.length).foreach {
      index =>
        if (word1(index) != word2(index)) {
          difference = difference + 1
        }
    }

    difference == 1
  }

  def main(args: Array[String]) {
    if (args.size == 0 || args.size > 2) {
      println("You must supply either a file name, or a start and end word")
      sys.exit(-1)
    }

    if (args.size == 1) {
      val source = Source.fromFile(args(0))
      // Save the puzzles as a Vector so that when they are parallelized they keep their order
      val puzzles = Vector[String](source.getLines().toSeq: _*)
      source.close()

      // Find the solution for each puzzle
      val solutions = puzzles.par.map(_.split("\t")).filter(validPuzzle(_)).map {
        words =>
          constructPath(words(0).toLowerCase, words(1).toLowerCase).getOrElse("")
      }.seq

      val out = new java.io.FileWriter("results.txt")
      solutions.foreach(solution => out.write(solution + "\n"))
      out.close()
    } else {
      if (validPuzzle(args)) {
        constructPath(args(0).toLowerCase, args(1).toLowerCase).map(println(_)).getOrElse {
          println("Path not found")
        }
      }
    }
  }

  // Build a path, if one exists, between two words
  def constructPath(startWord: String, endWord: String): Option[String] = {
    val endNode = checkLevel(Seq(new Node(None, startWord)), endWord, HashSet.empty[String])
    endNode.map(buildPathFromEndNode(_))
  }

  // Checks a given level of nodes to see if the wordToFind is present. If it isn't, move onto the next level
  @tailrec
  def checkLevel(nodes: Iterable[Node], wordToFind: String, alreadyChecked: Set[String]): Option[Node] = {
    nodes.find(_.data == wordToFind) match {
      case n@Some(node) => n
      case _ =>
        val nextLevel = getChildNodes(nodes, alreadyChecked)
        if (nextLevel.isEmpty) {
          None
        } else {
          checkLevel(nextLevel, wordToFind, alreadyChecked)
        }
    }
  }

  // Finds all children of the given nodes, ignoring ones that have already been checked as part of this puzzle
  def getChildNodes(nodes: Iterable[Node], alreadyChecked: Set[String]) = {
    val childNodes = new HashSet[Node]()
    nodes.foreach {
      node => dictionary.get(node.data).map(_.filterNot(alreadyChecked.contains(_)).foreach {
        neighbour =>
          alreadyChecked.add(neighbour)
          childNodes.add(new Node(Some(node), neighbour))
      })
    }

    childNodes
  }

  // A valid puzzle is one where both words are in the dictionary and have the correct size
  def validPuzzle(words: Array[String]): Boolean = {
    val result = words.filter(word => word.size == ValidWordLength && dictionary.contains(word)).size == 2
    if (!result) {
      println("Puzzle '" + words.mkString("-") + "' is invalid")
    }
    result
  }

  // Walk the node path back from the end node
  def buildPathFromEndNode(endNode: Node) = {
    new NodeIterable(endNode).toList.map(_.data).reverse.mkString(" -> ")
  }
}

class Node(val parent: Option[Node], val data: String) {

  override def equals(p1: Any) = p1.isInstanceOf[Node] && p1.asInstanceOf[Node].data == data

  override def hashCode() = data.hashCode

  override def toString = data
}

class NodeIterable(startNode: Node) extends Iterable[Node] {
  def iterator = new NodeIterator(startNode)
}

class NodeIterator(startNode: Node) extends Iterator[Node] {
  var currentNode: Node = null

  def hasNext = currentNode == null || currentNode.parent.isDefined

  def next() = {
    if (currentNode == null) {
      currentNode = startNode
    } else {
      currentNode = currentNode.parent.get
    }
    currentNode
  }
}
