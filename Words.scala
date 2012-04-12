import annotation.tailrec
import collection.mutable.{HashSet, Set}
import scala.io.Source

object Words {

  private val WordLength = 4

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

  private def loadDictionary: Map[String, Iterable[String]] = {

    var startTime = System.currentTimeMillis()

    val dict = Source.fromFile("/usr/share/dict/words")
    val words = dict.getLines().filter(_.length == WordLength).map(_.toLowerCase).toSet.toIterable

    println("Dictionary (" + words.size + " words) loaded in: " + (System.currentTimeMillis() - startTime).abs + "ms")

    startTime = System.currentTimeMillis()

    val resultMap = words.map {
      word =>
        word -> words.filter(areNeighbours(word, _))
    }.toMap

    println("Map (" + resultMap.size + " words) built in: " + (System.currentTimeMillis() - startTime).abs + "ms")

    dict.close()

    resultMap
  }

  private lazy val dictionary: Map[String, Iterable[String]] = loadDictionary

  def main(args: Array[String]) {
    if (args.size == 0 || args.size > 2) {
      println("You must supply either a file name, or a start and end word")
      sys.exit(-1)
    }

    if (args.size == 1) {
      val source = Source.fromFile(args(0))
      val puzzles = source.getLines()

      puzzles.foreach {
        puzzle =>
          val words = puzzle.split(" ")
          if (validPuzzle(words)) {
            constructPath(words(0).toLowerCase, words(1).toLowerCase).map(println(_)).getOrElse {
              println("Path not found")
            }
            constructPath(words(0).toLowerCase, words(1).toLowerCase).map(println(_)).getOrElse {
              println("Path not found")
            }
            constructPath(words(0).toLowerCase, words(1).toLowerCase).map(println(_)).getOrElse {
              println("Path not found")
            }
          } else {
            println("Skipping invalid puzzle: " + puzzle)
          }
      }

      source.close()
    } else {
      if (validPuzzle(args)) {
        constructPath(args(0).toLowerCase, args(1).toLowerCase).map(println(_)).getOrElse {
          println("Path not found")
        }
      } else {
        println("Puzzle is invalid")
      }
    }
  }

  def validPuzzle(words: Array[String]): Boolean = words.filter(_.size == 4).size == 2

  def constructPath(startWord: String, endWord: String): Option[String] = {

    println("Constructing a path from " + startWord + " to " + endWord)

    val startTime = System.currentTimeMillis()

    val endNode = checkLevel(Seq(new Node(None, startWord)), endWord, HashSet.empty[String])

    println("Finished searching in: " + (System.currentTimeMillis() - startTime).abs + "ms")

    endNode.map(buildPathFromEndNode(_))
  }

  def buildPathFromEndNode(endNode: Node) = {
    new NodeIterable(endNode).toList.map(_.data).reverse.foldLeft("") {
      (tmp: String, elem: String) => tmp + " -> " + elem
    }
  }

  @tailrec
  def checkLevel(nodes: Iterable[Node], wordToFind: String, alreadyChecked: Set[String]): Option[Node] = {
    nodes.find(_.data == wordToFind) match {
      case Some(node) =>
        Some(node)
      case _ =>
        val nextLevel = getChildNodes(nodes, alreadyChecked)
        if (nextLevel.isEmpty) {
          None
        } else {
          checkLevel(nextLevel, wordToFind, alreadyChecked)
        }
    }
  }

  def getChildNodes(nodes: Iterable[Node], alreadyChecked: Set[String]) = {
    val childNodes = new HashSet[Node]()
    nodes.foreach {
      node => dictionary.get(node.data).get.filterNot(alreadyChecked.contains(_)).foreach {
        neighbour =>
          alreadyChecked.add(neighbour)
          childNodes.add(new Node(Some(node), neighbour))
      }
    }

    childNodes
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
