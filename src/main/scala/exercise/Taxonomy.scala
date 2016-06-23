package exercise

import scala.collection.immutable.ListMap
import scala.collection.immutable.Stream._
import scala.util.matching.Regex

case class Tree(id: String, tag: Tag, children: Stream[Tree]) {

  private def toCSV: String = s"""$id,${tag.toCSV},"${children.map(_.id).mkString(",")}""""

  def findByTag(tag: String): Stream[Tree] = findDescendants.filter(_.tag.value == tag)

  def findDescendants: Stream[Tree] = children #::: children.flatMap(_.findDescendants)

  def cvsFormat: String = (this #:: findDescendants).map(e => e.toCSV).mkString("\n")

  def findById(id: String): Option[Tree] = (this #:: findDescendants).find(_.id == id)

}

object Tree {

  val pattern = new Regex("""^(\w+),(\w+),([\w+,\w+]+),"([\w?[,\w+]*]*)"$""", "id", "tag", "translations", "children")

  def leaf(id: String, tag: Tag) = Tree(id, tag, Stream.empty[Tree])

  def apply(csv: String): Tree = {

    def buildNode(csv: String, nodes: Map[String, Tree]) = {

      def children(childrenAsString: String, nodes: Map[String, Tree]): Stream[Tree] = {
        val c = childrenAsString.split(",").toList.filterNot(_ == "")
        c.map(n => nodes.getOrElse(n, throw new CVSFormatException(s"Node '$n' does not exist"))).toStream
      }

      pattern findFirstMatchIn csv match {
        case Some(s) => Tree(
          s.group("id"),
          Tag(s.group("tag"), s.group("translations")),
          children(s.group("children"), nodes)
        )
        case None => throw new CVSFormatException("The format is not correct")
      }
    }

    csv.lines.foldRight(ListMap.empty[String, Tree]) {
      (nodeCSV, nodes) =>
        val node = buildNode(nodeCSV, nodes)
        nodes + (node.id -> node)
    }.last._2
  }
}
