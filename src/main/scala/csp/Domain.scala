package csp

trait Variable
trait Value

object Domain {
  type VarValMap = Map[Variable, Set[Value]]
  def apply(aMap:VarValMap) = new Impl(aMap).asInstanceOf[Domain]

  private class Impl(aMap:VarValMap) extends Domain {
    override def contents = aMap
    override def remove(x_v:VarValMap):Option[Domain] = {
      var changed=false
      val res = aMap.flatMap { case (x,v) =>
        x_v.get(x).map{ v1 =>
          val res = v diff v1
          if (res.size < v.size) changed = true
          (x,res)
        }
      }
      if (changed)
        Some(new Impl(res))
      else
        None
    }

    override def bind(x_v:Map[Variable, Value]):Option[Domain] = {
      var newMap = aMap
      val iter = x_v.iterator
      while (iter.hasNext) {
        val (x,v) = iter.next()
        if(!newMap(x)(v))
          return None
        newMap = newMap.updated(x, Set(v))
      }
      if (newMap == aMap)
        Some(this)
      else
        Some(new Impl(newMap))
    }
    override def valWeight(x:Variable)(v:Value):Option[Int] = Some(contents(x).size)
  }
}

import Domain._

trait Domain {
  def contents:VarValMap
  def vars = contents.keySet
  //return None if nothing to remove
  def remove(x_v:VarValMap):Option[Domain]
  def bind(x_v:Map[Variable, Value]):Option[Domain]
  def valWeight(x:Variable)(v:Value):Option[Int]

  def print() = println(contents)
}
