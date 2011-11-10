package com.siniatech.siniagraph

import org.scalatest.junit._
import org.junit.Test
import org.junit.Before
import org.junit.Assert._

class GraphTest extends JUnitSuite with ShouldMatchersForJUnit {

  val va = "a"
  val vb = "b"
  val vc = "c"
  val vd = "d"
  val ve = "e"
  val vf = "f"
  val vg = "g"
  val vh = "h"

  val acyclic = Graph(Set(va, vb, vc, vd, ve, vf, vg, vh), EdgeSet(va -> vb, va -> vc, vb -> vd, vb -> ve, vc -> vf, ve -> vh, vd -> vh, ve -> vf, ve -> vg))
  val cyclic = Graph(Set(va, vb, vc, vd), EdgeSet(va -> vb, va -> vc, vb -> vd, vc -> vd, vd -> va))

  @Test
  def cantCreateDodgyGraph {
    intercept[IllegalArgumentException] {
      Graph(Set(va, vb), EdgeSet(va -> vb, va -> vc))
    }
  }

  @Test
  def checkInEdges {
    acyclic.inEdges(vb) should equal(EdgeSet(va -> vb))
    acyclic.inEdges(vf) should equal(EdgeSet(vc -> vf, ve -> vf))
    cyclic.inEdges(vb) should equal(EdgeSet(va -> vb))
  }

  @Test
  def checkOutEdges {
    acyclic.outEdges(va) should equal(Set(Edge(va -> vb), Edge(va -> vc)))
    acyclic.outEdges(ve) should equal(Set(Edge(ve -> vf), Edge(ve -> vh), Edge(ve -> vg)))
    acyclic.outEdges(vg) should equal(Set())
    cyclic.outEdges(vb) should equal(Set(Edge(vb -> vd)))
  }

  @Test
  def checkRoots {
    acyclic.roots should equal(Set(va))
    cyclic.roots should equal(Set())
  }

  @Test
  def checkLeaves {
    acyclic.leaves should equal(Set(vg, vh, vf))
    cyclic.leaves should equal(Set())
  }

  @Test
  def checkDescendants {
    acyclic.descendants(vb) should equal(Set(vd, ve, vf, vg, vh))
    cyclic.descendants(va) should equal(Set(va, vb, vc, vd))
  }

  @Test
  def checkDescendants_tr {
    acyclic.descendants_tr(vb) should equal(Set(vd, ve, vf, vg, vh))
    cyclic.descendants_tr(va) should equal(Set(va, vb, vc, vd))
  }

  @Test
  def checkAncestors {
    acyclic.ancestors(ve) should equal(Set(va, vb))
    GraphEditor().createAndAddEdge(va -> vb, vb -> vc, vc -> vd, ve -> vf, vf -> vd)().ancestors(vd) should equal(Set(va, vb, vc, ve, vf))
    cyclic.ancestors(va) should equal(Set(va, vb, vc, vd))
  }

  @Test
  def checkAncestors_tr {
    acyclic.ancestors_tr(ve) should equal(Set(va, vb))
    GraphEditor().createAndAddEdge(va -> vb, vb -> vc, vc -> vd, ve -> vf, vf -> vd)().ancestors_tr(vd) should equal(Set(va, vb, vc, ve, vf))
    cyclic.ancestors_tr(va) should equal(Set(va, vb, vc, vd))
  }

  @Test
  def checkCyclicity {
    acyclic.isCyclic should be(false)
    cyclic.isCyclic should be(true)
  }

  @Test
  def checkAdd {
    GraphEditor().addVertex(va).graph should equal(Graph(Set(va), Set[Edge[String]]()))
    GraphEditor().addVertex(va).addVertex(vb).graph should equal(Graph(Set(va, vb), Set[Edge[String]]()))
    GraphEditor().addVertex(va).addVertex(vb).addEdge(Edge(va -> vb)).graph should equal(Graph(Set(va, vb), Set(Edge(va -> vb))))
    GraphEditor[String]().addEdge(Edge(va -> vb)).graph should equal(Graph(Set(va, vb), Set(Edge(va -> vb))))
    GraphEditor(cyclic).addVertex(vg).graph should equal(Graph(Set(va, vb, vc, vd, vg), EdgeSet(va -> vb, va -> vc, vb -> vd, vc -> vd, vd -> va)))
  }

  @Test
  def checkLoneVertices {
    Graph(Set(va, vb, vc), Set(Edge(va -> vb))).loneVertices should equal(Set(vc))
    cyclic.loneVertices should equal(Set())
  }

  @Test
  def checkRemove {
    GraphEditor().addVertex(va).removeVertex(va).graph should equal(Graph(Set(), Set[Edge[String]]()))
    GraphEditor().addVertex(va).addVertex(vb).removeVertex(va).graph should equal(Graph(Set[String](vb), Set[Edge[String]]()))
    GraphEditor(cyclic).removeVertex(va).graph should equal(Graph(Set(vb, vc, vd), Set(Edge(vb -> vd), Edge(vc -> vd))))
    GraphEditor(cyclic).removeEdge(Edge(vb -> vd)).graph should equal(Graph(Set(va, vb, vc, vd), Set(Edge(va -> vb), Edge(va -> vc), Edge(vc -> vd), Edge(vd -> va))))
  }

  @Test
  def checkAddEdge {
    GraphEditor().createAndAddEdge((va, vb)).graph should equal(Graph(Set[String](va, vb), Set[Edge[String]](Edge(va -> vb))))
    GraphEditor().addVertex(va).createAndAddEdge((va, vb)).graph should equal(Graph(Set[String](va, vb), Set[Edge[String]](Edge(va -> vb))))
    GraphEditor().createAndAddEdge(va -> vb, vb -> vc, va -> "z").graph should equal(Graph(Set[String](va, vb, vc, "z"), Set[Edge[String]](Edge(va, vb), Edge(vb, vc), Edge(va, "z"))))
  }

  @Test
  def checkFindPaths {
    GraphEditor().createAndAddEdge((va, vb)).createAndAddEdge((vb, vc)).createAndAddEdge((vc, vd)).
      graph.findPaths(va, vd) should equal(Set(List(va, vb, vc, vd)))
    GraphEditor().createAndAddEdge((va, vb), (vb, vc), (vc, vd), (va, ve), (ve, vf), (vf, vd)).
      graph.findPaths(va, vd) should equal(Set(List(va, vb, vc, vd), List(va, ve, vf, vd)))
    GraphEditor().createAndAddEdge((va, vb), (vb, vc), (vc, vd), (va, ve), (ve, vf)).
      graph.findPaths(va, vd) should equal(Set(List(va, vb, vc, vd)))
    GraphEditor().createAndAddEdge((va, vb), (vb, vc), (vc, vd), (ve, vf), (vf, vd)).
      graph.findPaths(va, vd) should equal(Set(List(va, vb, vc, vd)))
    intercept[IllegalArgumentException] {
      cyclic.findPaths(va, vd)
    }
  }

  @Test
  def checkFindPaths_tr {
    GraphEditor().createAndAddEdge((va, vb)).createAndAddEdge((vb, vc)).createAndAddEdge((vc, vd)).
      graph.findPaths_tr(va, vd) should equal(Set(List(va, vb, vc, vd)))
    GraphEditor().createAndAddEdge((va, vb), (vb, vc), (vc, vd), (va, ve), (ve, vf), (vf, vd)).
      graph.findPaths_tr(va, vd) should equal(Set(List(va, vb, vc, vd), List(va, ve, vf, vd)))
    GraphEditor().createAndAddEdge((va, vb), (vb, vc), (vc, vd), (va, ve), (ve, vf)).
      graph.findPaths_tr(va, vd) should equal(Set(List(va, vb, vc, vd)))
    GraphEditor().createAndAddEdge(va -> vb, vb -> vc, vc -> vd, ve -> vf, vf -> vd).
      graph.findPaths_tr(va, vd) should equal(Set(List(va, vb, vc, vd)))
    cyclic.findPaths_tr(va, vd) should equal(Set(List(va, vc, vd), List(va, vb, vd)))
    GraphEditor().createAndAddEdge(va -> vb, vb -> vb)().findPaths_tr(va, vb) should equal(Set(List(va, vb)))
    GraphEditor().createAndAddEdge(va -> vb, vc -> vb, vb -> vc)().findPaths_tr(va, vb) should equal(Set(List(va, vb)))
    GraphEditor().createAndAddEdge(va -> vb, vc -> vb, vb -> vc)().findPaths_tr(vc, vb) should equal(Set(List(vc, vb)))
    GraphEditor().createAndAddEdge(va -> vb, vb -> vc, vc -> vd, va -> vc)().findPaths_tr(va, vd) should equal(Set(List(va, vc, vd), List(va, vb, vc, vd)))
  }

  @Test
  def checkShortestPath {
    GraphEditor().createAndAddEdge(va -> vb, vb -> vc, vc -> vd, va -> vc)().shortestPath(va, vd) should equal(List(va, vc, vd))
    GraphEditor().createAndAddEdge(va -> vb, vb -> vc, vc -> vd, va -> vc, va -> vd)().shortestPath(va, vd) should equal(List(va, vd))
    GraphEditor().createAndAddEdge(va -> vb, vb -> vc, vc -> vd, va -> vc, va -> ve, ve -> vd)().
      shortestPath(va, vd) should (equal(List(va, vc, vd)) or equal(List(va, ve, vd)))
  }

  @Test
  def checkApply {
    val gb = GraphEditor().createAndAddEdge((va, vb), (vb, vc), (vc, vd))
    gb.graph should equal(gb())
    gb() should equal(gb.graph)
    gb() should equal(Graph(Set(va, vb, vc, vd), EdgeSet(va -> vb, vb -> vc, vc -> vd)))
  }

  @Test
  def checkMerge {
    val g1 = GraphEditor((va, vb), (vb, vc), (vc, vd))()
    val g2 = GraphEditor((va, ve), (ve, vf), (vf, vd))()
    GraphEditor(g1).merge(g2)() should equal(GraphEditor().createAndAddEdge((va, vb), (vb, vc), (vc, vd), (va, ve), (ve, vf), (vf, vd))())
    GraphEditor(g1).merge(g2)() should equal(GraphEditor(g2).merge(g1)())
    GraphEditor(g1).merge(Graph())() should equal(g1)
    GraphEditor(Graph[String]()).merge(g1)() should equal(g1)
  }

  @Test
  def checkReplaceEdge {
    Edge(va -> vb).replace(va -> vc) should equal(Edge(vc -> vb))
    Edge(va -> vb).replace(va -> vc, vb -> vd) should equal(Edge(vc -> vd))
    Edge(va -> vb).replace(vb -> vd) should equal(Edge(va -> vd))
    intercept[IllegalArgumentException] {
      Edge(va -> vb).replace(vc -> vd)
    }
  }

  @Test
  def checkReplaceGraph {
    GraphEditor(va -> vb, va -> vc).replace(va -> vd)() should equal(Graph(Set(vb, vc, vd), EdgeSet(vd -> vb, vd -> vc)))
    GraphEditor(va -> vb, va -> vc, vb -> vd, vd -> va).replace(va -> vd)() should equal(Graph(Set(vb, vc, vd), EdgeSet(vd -> vb, vd -> vc, vd -> vd, vb -> vd)))
    GraphEditor(va -> vb, va -> vc, vb -> vd, vd -> va).replace(va -> ve)() should equal(Graph(Set(vb, vc, vd, ve), EdgeSet(ve -> vb, ve -> vc, vb -> vd, vd -> ve)))
  }

  @Test
  def checkFlip {
    GraphEditor().createAndAddEdge(va -> vb, va -> vc, vb -> vd, vd -> va).flip()() should equal(GraphEditor().createAndAddEdge(vb -> va, vc -> va, vd -> vb, va -> vd)())
    GraphEditor(cyclic).flip()() should equal(Graph(Set(va, vb, vc, vd), EdgeSet(vb -> va, vc -> va, vd -> vb, vd -> vc, va -> vd)))
  }

  @Test
  def checkIslandFor {
    GraphEditor(va -> vb, vc -> vd)().islandFor(va) should equal(GraphEditor().createAndAddEdge(va -> vb)())
    GraphEditor(va -> vb)().islandFor(va) should equal(GraphEditor().createAndAddEdge(va -> vb)())
    GraphEditor(va -> vb, vc -> vd, ve -> vf)().islandFor(va) should equal(GraphEditor().createAndAddEdge(va -> vb)())
    GraphEditor(va -> vb, va -> vf, vf -> va, vc -> vd, ve -> vc)().islandFor(va) should equal(GraphEditor(va -> vb, va -> vf, vf -> va)())
    GraphEditor(va -> vb, va -> vf, vf -> va, vc -> vd, ve -> vc)().islandFor(vc) should equal(GraphEditor(vc -> vd, ve -> vc)())
    GraphEditor(va -> vb, va -> vf, vf -> va, vc -> vd, ve -> vc)().islandFor(vb) should equal(GraphEditor(va -> vb, va -> vf, vf -> va)())
    GraphEditor(va -> vb, va -> vf, vf -> va, vc -> vd, ve -> vc)().islandFor(vd) should equal(GraphEditor(vc -> vd, ve -> vc)())
    GraphEditor(va -> vb, va -> vf, vf -> va, vc -> vd, ve -> vc)().islandFor(ve) should equal(GraphEditor(vc -> vd, ve -> vc)())
    intercept[IllegalArgumentException] {
      GraphEditor(va -> vb, vc -> vd)().islandFor(ve)
    }
  }

  @Test
  def checkSplit {
    GraphEditor(va -> vb, vc -> vd).split.map(_.graph) should equal(Set(GraphEditor(va -> vb)(), GraphEditor(vc -> vd)()))
    GraphEditor(va -> vb, vc -> vd, ve -> vf).split.map(_.graph) should equal(Set(GraphEditor(va -> vb)(), GraphEditor(vc -> vd)(), GraphEditor(ve -> vf)()))
    GraphEditor(va -> vb, va -> vf, vf -> va, vc -> vd, ve -> vc).split.map(_.graph) should equal(Set(GraphEditor(va -> vb, va -> vf, vf -> va)(), GraphEditor(vc -> vd, ve -> vc)()))
  }
}
