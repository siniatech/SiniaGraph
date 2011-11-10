package com.siniatech.siniagraph

import scala.annotation.tailrec
import scala.collection

class Graph[V](vs: Set[V], es: Set[Edge[V]]) {
  require(es.map(e => Set(e.source, e.target)).flatten.forall(vs.contains _), "Can't have edges to vertices not in the graph:" + vs + "," + es)

  def vertices: Set[V] = vs
  def edges: Set[Edge[V]] = es
  def inEdges(v: V): Set[Edge[V]] = edges.filter(_.target == v)
  def outEdges(v: V): Set[Edge[V]] = edges.filter(_.source == v)
  def roots: Set[V] = vertices.filter(inEdges(_).size == 0)
  def leaves: Set[V] = vertices.filter(outEdges(_).size == 0)
  def children(v: V): Set[V] = outEdges(v).map(_.target)
  def parents(v: V): Set[V] = inEdges(v).map(_.source)
  def isCyclic: Boolean = vertices.exists(v => ancestors(v).contains(v))
  def loneVertices = vertices.filterNot(v => edges.exists(e => e.exists(_ == v)))

  /**
   * Stops at self if cyclic
   */
  def ancestors(v: V): Set[V] = ancDec(v, v => parents(v))
  def descendants(v: V): Set[V] = ancDec(v, v => children(v))

  private def ancDec(start: V, f: V => Set[V]): Set[V] = {
    require(vertices contains start)
    def safeAncestors(v: V): Set[V] = {
      f(v).flatMap(x => if (x == start) Set(x) else safeAncestors(x) + x)
    }
    safeAncestors(start)
  }

  def descendants_tr(v: V): Set[V] = ancDec_tr(v, v => children(v));
  def ancestors_tr(v: V): Set[V] = ancDec_tr(v, v => parents(v));

  private def ancDec_tr(start: V, f: V => Set[V]): Set[V] = {
    require(vertices contains start)

    @tailrec
    def ancDecRec(toBeDone: Set[V], current: Set[V]): Set[V] = {
      toBeDone match {
        case s if s.size == 0 => current
        case _ => f(toBeDone.head).toList match {
          case Nil => ancDecRec(toBeDone - toBeDone.head, current)
          case head :: tail if head == start => ancDecRec(toBeDone.tail, current + head)
          case l => ancDecRec(toBeDone.tail ++ l, current ++ l)
        }
      }
    }
    ancDecRec(Set(start), Set[V]())
  }

  import PartialFunction.cond

  override def equals(other: Any): Boolean = {
    cond(other) {
      case o: Graph[V] => vertices == o.vertices && edges == o.edges
    }
  }

  override def hashCode(): Int = 31 * vertices.hashCode * edges.hashCode
  override def toString(): String = {
    val sb = new StringBuilder
    sb.append("{")
    loneVertices.addString(sb, ",")
    sb.append(";")
    edges.addString(sb, ",")
    sb.append("}")
    sb.toString
  }

  // acyclic only
  def findPaths(from: V, to: V): Set[List[V]] = {
    require(!isCyclic)
    require(vertices contains from)
    require(vertices contains to)

    def safeFindPath(to: V, current: List[V]): Set[List[V]] = {
      from match {
        case _ if from == to => Set(from :: current)
        case _ => inEdges(to).map(_.source).flatMap(v => safeFindPath(v, to :: current))
      }
    }
    safeFindPath(to, List())
  }

  // includes cycles
  def findPaths_tr(from: V, to: V): Set[List[V]] = {
    require(vertices contains from)
    require(vertices contains to)

    @tailrec
    def safeFindPath_tr(possPaths: Set[List[V]], completePaths: Set[List[V]]): Set[List[V]] = {
      possPaths match {
        case s if s.size == 0 => completePaths
        case _ => {
          val possPath = possPaths.iterator.next
          possPath match {
            case head :: tail if head == from => safeFindPath_tr(possPaths - possPath, completePaths + possPath)
            case head :: tail if parents(head).isEmpty || (head == to && tail != Nil) =>
              safeFindPath_tr(possPaths - possPath, completePaths)
            case head :: tail => {
              val newPossPaths = possPaths ++ parents(head).map(p => p :: possPath) - possPath
              safeFindPath_tr(newPossPaths, completePaths)
            }
            case Nil => throw new IllegalStateException
          }
        }
      }
    }
    safeFindPath_tr(Set(List(to)), Set())
  }

  def shortestPath(from: V, to: V): List[V] = {
    findPaths_tr(from, to).foldLeft(Nil: List[V])((a, b) => if (a.size < b.size && a.size > 0) a else b)
  }

  def islandFor(v: V): Graph[V] = {
    require(vertices contains v)
    @tailrec
    def allConnected(toDo: Set[V], current: Set[V]): Set[V] = {
      toDo.toList match {
        case Nil => current
        case head :: tail => allConnected(parents(head) ++ children(head) ++ tail -- current, current + head)
      }
    }
    val connectedVertices: Set[V] = allConnected(Set(v), Set())
    Graph(connectedVertices, edges.filter(e => e.iterator.exists(connectedVertices.contains)))
  }
}

object GraphEditor {
  def apply[V](): GraphEditor[V] = new GraphEditor(Graph())
  def apply[V](g: Graph[V]): GraphEditor[V] = new GraphEditor(g)
  def apply[V](e: (V, V)*): GraphEditor[V] = GraphEditor().createAndAddEdge(e: _*)
}
class GraphEditor[V](g: Graph[V]) {
  def addVertex(v: V*): GraphEditor[V] = GraphEditor(Graph(g.vertices ++ v, g.edges))
  def addEdge(e: Edge[V]*): GraphEditor[V] = GraphEditor(Graph(g.vertices ++ e.flatten, g.edges ++ e))
  def createAndAddEdge(pairs: (V, V)*): GraphEditor[V] = addEdge(pairs.map(pair => { val (s, t) = pair; Edge(s, t) }): _*)
  def removeVertex(v: V): GraphEditor[V] = {
    require(graph.vertices contains v)
    GraphEditor(Graph(g.vertices - v, g.edges.filterNot(e => e.exists(_ == v))))
  }
  def removeEdge(e: Edge[V]): GraphEditor[V] = {
    require(graph.edges contains e)
    GraphEditor(Graph(g.vertices, g.edges - e))
  }
  def graph: Graph[V] = g
  def apply(): Graph[V] = graph
  def merge(other: Graph[V]): GraphEditor[V] = GraphEditor(Graph(g.vertices ++ other.vertices, g.edges ++ other.edges))
  def replace(p: (V, V)*): GraphEditor[V] = {
    require(p.map(_._1).forall(graph.vertices.contains _))
    val repEdgeFilter = { e: Edge[V] => e.iterator.exists(p.map(_._1) contains _) }
    GraphEditor(Graph(g.vertices -- p.map(_._1) ++ p.map(_._2),
      g.edges.filterNot(repEdgeFilter) ++ g.edges.filter(repEdgeFilter).map(e => e.replace(p.filter(e.iterator contains _._1): _*))))
  }
  def flip(): GraphEditor[V] = GraphEditor(Graph(g.vertices, g.edges.map(_.flip)))
  def split(): Set[GraphEditor[V]] = g.vertices.map(v => g.islandFor(v)).map(GraphEditor(_))
}

object Graph {
  def apply[V](vs: Set[V], es: Set[Edge[V]]): Graph[V] = new Graph(vs, es)
  def apply[V](): Graph[V] = new Graph(Set(), Set())
}

object EdgeSet {
  def apply[V](p: (V, V)*): Set[Edge[V]] = p.map(p => { val (s, t) = p; Edge(s, t) }).toSet
}

object Edge {
  def apply[V](s: V, t: V): Edge[V] = new Edge(s, t)
  def apply[V](p: (V, V)): Edge[V] = { val (s, t) = p; Edge(s, t) }
}

class Edge[V](s: V, t: V) extends Iterable[V] {
  def source: V = s
  def target: V = t
  override def toString: String = { val sb = new StringBuilder; iterator.addString(sb, "->"); sb.toString }
  override def iterator = Iterator(source, target)
  override def equals(other: Any) = {
    other match {
      case o: Edge[V] => source == o.source && target == o.target
      case _ => false
    }
  }
  override def hashCode(): Int = 31 * source.hashCode * target.hashCode
  def replace(p: (V, V)*): Edge[V] = {
    require(p.map(_._1).forall(iterator.contains _))
    def convert(v: V): V = if (p.map(_._1).contains(v)) p.find(_._1 == v).get._2 else v
    Edge(convert(source), convert(target))
  }
  def flip(): Edge[V] = Edge(target, source)
}
