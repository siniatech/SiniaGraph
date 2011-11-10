package com.siniatech.siniagraph

object GraphLauncher {
  def main(args: Array[String]): Unit = {
    val a = "a"
    val b = "b"
    val c = "c"
    val d = "d"
    val e = "e"
    val f = "f"
    val g = "g"
    val h = "h"

    val ab = Edge(a, b)
    val ac = Edge(a, c)
    val bd = Edge(b, d)
    val be = Edge(b, e)
    val ef = Edge(e, f)
    val dh = Edge(d, h)
    val cf = Edge(c, f)

    val graph = Graph(Set(a, b, c, d, e, f, g, h), Set(ab, ac, bd, be, cf, ef, dh))

    println(graph.inEdges(b))
    println(graph.outEdges(b))
    println(graph.roots)
    println(graph.ancestors(b))
    println(graph.descendants(b))
  }
}
