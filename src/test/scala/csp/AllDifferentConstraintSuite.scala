package csp

import org.scalatest.FunSuite
import csp.constraint.AllDifferentConstraint

class AllDifferentConstraintSuite extends FunSuite {
  val ad = new AllDifferentConstraint
  object x extends Variable
  object y extends Variable
  object z extends Variable
  object one extends Value
  object two extends Value
  object three extends Value
  object four extends Value

  test ("full graph is feasible, nothing to prune") {
    val d1 = Domain(Map(x->Set(one, two), y->Set(one, two)))
    val chad1 = ad.check(d1)

    assert(chad1.isFeasible)
    assert(chad1.prune.isEmpty)

  }

  test ("too few values, not feasible, prune is illegal") {
    val d2 = Domain(Map(x->Set(one, two), y->Set(one, two), z->Set(one, two)))
    val chad2 = ad.check(d2)
    assert(!chad2.isFeasible)
    intercept[UnsupportedOperationException] {
      chad2.prune
    }
  }

  test ("forced value on var, other var for that val is pruned") {
    val d = Domain(Map(x->Set(one), y->Set(one, two)))
    val ch = ad.check(d)
    assert(ch.isFeasible)
    assert(ch.prune === Map(y->Set(one)))
  }

  test ("slow feasible") {
    val vals = Array.tabulate(1001)(i => new Value{}).toSet
    val vars = Array.tabulate(1000)(i => new Variable {})
    val bigDomain = Domain(vars.map(v => (v, vals)).toMap)
    println ("before big check")
    val bigCheck = ad.check(bigDomain)
    println("all different big ="+bigCheck.isFeasible)
  }

  test ("slow infeasible") {
    val vals = Array.tabulate(1000)(i => new Value{}).toSet
    val vars = Array.tabulate(1001)(i => new Variable {})
    val bigDomain = Domain(vars.map(v => (v, vals)).toMap)
    println ("before big check")
    val bigCheck = ad.check(bigDomain)
    println("all different big ="+bigCheck.isFeasible)
  }

  test ("example from lecture") {
    val vars = Array.tabulate[Variable](6)(i => new Variable {override def toString="var"+(i+1)})
    val vals = Array.tabulate[Value](7)(i => new Value {override def toString="val"+(i+1)})

    val d = Domain(Map(
      vars(0)->Set(vals(0),vals(1)),
      vars(1)->Set(vals(1),vals(2)),
      vars(2)->Set(vals(0),vals(2)),
      vars(3)->Set(vals(1),vals(3)),
      vars(4)->Set(vals(2),vals(3),vals(4),vals(5)),
      vars(5)->Set(vals(5),vals(6))
    ))

    val ch = ad.check(d)

    assert(ch.isFeasible)
    assert(ch.prune === Map(
      vars(3)->Set(vals(1)),
      vars(4)->Set(vals(2),vals(3))
    ))
  }
}
