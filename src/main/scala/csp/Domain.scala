package csp

trait Variable
trait Value

object Domain {
  type VarValMap = Map[Variable, Set[Value]]
  def apply(aMap:VarValMap) = new Impl(aMap).asInstanceOf[Domain]

  private class Impl(aMap:VarValMap) extends Domain {
    def contents = aMap
    def remove(x_v:VarValMap):Option[Domain] = {
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
  }
}

import Domain._

trait Domain {
  def contents:VarValMap
  //return None if nothing to remove
  def remove(x_v:VarValMap):Option[Domain]
}
