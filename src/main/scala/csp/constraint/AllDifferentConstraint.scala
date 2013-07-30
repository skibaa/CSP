package csp.constraint

import csp._
import Domain._
import scala.annotation.tailrec

import scala.collection.mutable.{Map=>MutableMap,Set=>MutableSet,HashMap}
import scala.collection.mutable
import csp.constraint.Constraint.CheckResult

class Vertex {
  //Arrows from this
  var from = MutableSet.empty[Vertex]
  //Arrows to this
  var to =  MutableSet.empty[Vertex]

  var marked = false
  var isFree = true

  def reverse = {
    val t = from
    from = to
    to = t
  }
}
case class VarVertex(x:Variable) extends Vertex
case class ValVertex(vl:Value) extends Vertex

object Edge {
  def apply (from:Vertex, to:Vertex):Edge = (from, to) match {
    case (x:VarVertex, v:ValVertex) => new Edge(x, v).init(true)
    case (v:ValVertex, x:VarVertex) => new Edge(x, v).init(false)
    case _ => throw new IllegalArgumentException("Must be one argument of type VarVertex and another ValVertex, in any order")
  }
}

case class Edge private (_x:VarVertex, _v:ValVertex) {
  var marked = false
  private var _fromVar = true
  def fromVar = _fromVar
  def fromVar_= (fv:Boolean) {
    if (_fromVar == fv)
      return

    _fromVar = fv
    if(_fromVar)
      setFromTo(x, v)
    else
      setFromTo(v, x)
  }
  def init(fv:Boolean) = {
    fromVar = fv
    this
  }

  def from = if (fromVar) x else v
  def to = if(fromVar) v else x
  def x = _x
  def v = _v

  private def setFromTo(from:Vertex, to:Vertex) = {
    from.from += to
    from.to -= to
    to.to += from
    to.from -= from
  }

  def reverse = {fromVar = !fromVar}
}

case class Graph(vars:Set[VarVertex], vals:Set[ValVertex]) {
  def all:Iterator[Vertex] = vars.iterator ++ vals.iterator

  def reverse = all.foreach(_.reverse)

  private def findUnmarkedFrom(from:Vertex):Iterable[Vertex] = {
    val parts = from.from.view.partition(_.isFree)
    parts._1.filter(!_.marked) ++ parts._2.filter(!_.marked)
  }

  def forAllPaths(from:Vertex, f:Edge=>Unit):Unit = {
    val options = findUnmarkedFrom(from)
    if (options.isEmpty)
      return
    for(to <- options) {
      val edge = Edge(from, to)
      f(edge)
      forAllPaths(to, f)
    }
  }

  def findPath(from:Vertex, check:Edge=>Boolean) = {
    def doFindPath(from:Vertex, check:Edge=>Boolean, accPath:List[Edge]):Option[List[Edge]] = {
      val options = findUnmarkedFrom(from)
      if (options.isEmpty)
        return None

      val i = options.iterator
      while (i.hasNext) {
        val to = i.next()
        val newEdge = Edge(from, to)
        val newPath = newEdge :: accPath
        if (check(newEdge))
          return Some(newPath)
        to.marked = true
        val res = doFindPath(to, check, newPath)
        if (res.isDefined) {
          return res
        }
      }
      None
    }

    doFindPath(from, check, Nil)
  }

  def freeVars = vars.view.filter(_.isFree)

  def improve:Boolean = {
    if (freeVars.isEmpty)
      return false

    def isFreeValue(edge:Edge) = edge.to match {
      case vv @ ValVertex(_) if vv.isFree =>
        true
      case _ => false
    }

    unmarkAll

    val path = findPath(freeVars.minBy(_.from.size), isFreeValue)

    path match {
      case Some(edgesList) =>
        edgesList.foreach{edge =>
          edge.reverse
          edge.v.isFree = false
          edge.x.isFree = false
        }
        true
      case None =>
        false
    }
  }

  def unmarkAll {
    vars.foreach(_.marked = false)
    vals.foreach(_.marked = false)
  }

}

class AllDifferentConstraint(vars:Set[Variable]) extends Constraint {
  def check(d: Domain): CheckResult = new CheckResult {
    if (!vars.forall(d.contents.contains(_)))
      return Constraint.infeasible

    val graph = {
      val graphVals = MutableMap.empty[Value, ValVertex]
      val graphVars = MutableSet.empty[VarVertex]

      vars.foreach{x =>
        val vals = d.contents(x)
        val varVertex = VarVertex(x)

        vals.foreach { value =>
          val valVertex = graphVals.getOrElseUpdate(value, ValVertex(value))
          varVertex.from += valVertex
          valVertex.to += varVertex
        }
        graphVars += varVertex
      }
      Graph (vars=graphVars.toSet, vals = graphVals.values.toSet)
    }

    lazy val isFeasible: Boolean = {
      println("Start isFeasible")
      while (graph.improve)
        ()

      graph.freeVars.isEmpty
    }

    def prune: VarValMap = {
      if (!isFeasible)
        throw new UnsupportedOperationException("Cannot prune infeasible solution")

      graph.reverse

      val freeEdgesSet = for {
        from <- graph.vals
        to <- from.from
      }
        yield Edge(from, to)

      val freeEdgesMap = freeEdgesSet.map(edge => (edge, edge)).toMap

      graph.unmarkAll

      def forEachFreeEdge(mayBePath:Option[Iterable[Edge]], f:Edge => Unit) = for {
        path <- mayBePath
        edge <- path
        freeEdge <- freeEdgesMap.get(edge)
      } f(freeEdge)

      def markEvenPathsFromFree {
        val freeVals = graph.vals.filter(_.isFree)

        def mark(edge:Edge) = {
          freeEdgesMap.get(edge).foreach(e => e.marked = true)
        }

        freeVals.foreach{freeVal =>
          graph.forAllPaths(freeVal, mark)
        }
      }

      markEvenPathsFromFree

      val neededEdges:MutableSet[Edge] = MutableSet.empty
      neededEdges ++= freeEdgesSet.filter(_.marked)

      def findCircle(startEdge: Edge) = {
        def isCircle (edge:Edge) =
          (edge.to == startEdge.from)

        startEdge.to.marked = true
        graph.findPath(startEdge.to, isCircle).map(path => startEdge::path)
      }

      freeEdgesSet.foreach { edge =>
        if (!neededEdges(edge)) {
          graph.unmarkAll
          val mayBePath = findCircle(edge)
          forEachFreeEdge(mayBePath, freeEdge => neededEdges += freeEdge)
        }
      }

      val unneededEdges = freeEdgesSet -- neededEdges
      val res = new HashMap[Variable, MutableSet[Value]] with mutable.MultiMap[Variable, Value]
      unneededEdges.foreach(edge => res.addBinding(edge.x.x, edge.v.vl))
      res.map{case (k, v) => (k,v.toSet)}.toMap
    }

  }
}
