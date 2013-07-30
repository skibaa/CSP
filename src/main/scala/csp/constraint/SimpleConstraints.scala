package csp.constraint

import csp._
import Domain._
import csp.constraint.Constraint.CheckResult

object Condition {
  val empty = new Condition {
    def partition(values: Set[Value]): (Set[Value], Set[Value]) = (values, Set.empty)
  }
}

trait Condition {
  def partition(values:Set[Value]):(Set[Value],Set[Value])
}

trait SimpleCondition extends Condition {
  override def partition(values:Set[Value]):(Set[Value],Set[Value]) =
    values partition isGood

  def isGood(v:Value):Boolean
}

case class SingleVarConstraint(v:Variable, c:Condition) extends Constraint {
  def check(d: Domain) = new CheckResult {
    val (goods, bads) = c.partition(d.contents(v))
    override def isFeasible:Boolean = ! goods.isEmpty
    override def prune:VarValMap = if (bads.isEmpty) Map.empty else Map(v->bads)
  }
}

object Implicits {
  implicit def var2ops (v:Variable):VariableOps = new VariableOps(v)
  implicit def lc2ops (lc:SingleVarConstraint) = new LocalConstraintOps(lc)
  implicit def cond2ops (c:Condition):ConditionOps = new ConditionOps(c)
}

class VariableOps(x:Variable) {
  def in (vals:Set[Value]) = new SingleVarConstraint(x, new SimpleCondition {
    def isGood(vl:Value) = vals.contains(vl)
  })
  def not_in (vals:Set[Value]) = new SingleVarConstraint(x, new SimpleCondition {
    def isGood(vl:Value) = !vals.contains(vl)
  })
}

class ConditionOps(c:Condition) {
  def unary_! = new Condition {
    def partition(values: Set[Value]) = {
      val myPart = c.partition(values)
      (myPart._2, myPart._1)
    }
  }

  def && (that:Condition) = new Condition {
    def partition(values: Set[Value]): (Set[Value], Set[Value]) = {
      val myPart = c.partition(values)
      val thatPart = that.partition(myPart._1)
      (thatPart._1, thatPart._2 ++ myPart._2)
    }
  }

  def || (that:Condition) = new Condition {
    def partition(values: Set[Value]): (Set[Value], Set[Value]) = {
      val myPart = c.partition(values)
      val thatPart = that.partition(myPart._2)
      (myPart._1 ++ thatPart._1, thatPart._2)
    }
  }
}

class LocalConstraintOps(lc:SingleVarConstraint) {
  import Implicits._

  def unary_! = SingleVarConstraint(lc.v, ! lc.c)

  def && (that:SingleVarConstraint) = {
    assert (that.v == lc.v)
    SingleVarConstraint(lc.v, lc.c && that.c)
  }

  def || (that:SingleVarConstraint) = {
    assert (lc.v == that.v)
    SingleVarConstraint(lc.v, lc.c || that.c)
  }
}
