package nqueens

import org.scalatest.FunSuite
import csp.constraint.AllDifferentConstraint
import NQDomain._
import csp.{Solver, Value, Variable, Domain}
import java.lang.UnsupportedOperationException

class NQSuite extends FunSuite {
  test ("test 2x2 columns") {
    val d = NQDomain.empty(2)
    val adc = new AllDifferentConstraint(d.columns.toSet)
    val adcc = adc.check(d)
    assert(adcc.isFeasible)
    assert(adcc.prune.isEmpty)

    val d1 = d.remove(Map(d.columns(0)->Set(NQVal(1)))).get
    val adcc1 = adc.check(d1)
    assert(adcc1.isFeasible)
    assert(adcc1.prune == Map(d.columns(1)->Set(NQVal(0))))

    val d2 = d1.remove(adcc1.prune).get
    val adcc2 = adc.check(d2)
    assert(adcc2.isFeasible)
    assert(adcc2.prune.isEmpty)
  }

  test ("test 3x3 full infeasible") {
    val d = NQDomain.empty(3)
    val d1 = d.remove(Map(d.columns(0)->Set(NQVal(1),NQVal(2)))).get
    val adc = new AllDifferentConstraint(d.columns.toSet)
    val adu = new AllDifferentConstraint(d.diagup.toSet)
    val add = new AllDifferentConstraint(d.diagdown.toSet)

    val adcc = adc.check(d1)
    val aduc = adu.check(d1)
    val addc = add.check(d1)

    assert(adcc.isFeasible)
    assert(aduc.isFeasible)
    assert(addc.isFeasible)

    val d2 = d1.remove(adcc.prune ++ aduc.prune ++ addc.prune).get

    val adcc2 = adc.check(d2)
    val aduc2 = adu.check(d2)
    val addc2 = add.check(d2)

    assert(adcc2.isFeasible)
    assert(!aduc2.isFeasible)
    assert(addc2.isFeasible)
  }

  test ("4x4 corner infeasible") {
    val d = NQDomain.empty(4)
    val d1 = d.bind(Map(d.columns(0)->NQVal(0))).get

    val adc = new AllDifferentConstraint(d.columns.toSet)
    val adu = new AllDifferentConstraint(d.diagup.toSet)
    val add = new AllDifferentConstraint(d.diagdown.toSet)

    val solver = new Solver(d1, Seq(adc, adu, add))

    assert(solver.solve.isEmpty)
  }

  test ("4x4 feasible") {
    val d = NQDomain.empty(4)

    val adc = new AllDifferentConstraint(d.columns.toSet)
    val adu = new AllDifferentConstraint(d.diagup.toSet)
    val add = new AllDifferentConstraint(d.diagdown.toSet)

    val solver = new Solver(d, Seq(adc, adu, add))

    val solution = solver.solve

    assert(solution.isDefined)

    println("Found solution:")
    solution.get.print()
  }

  test ("big feasible") {
    val solution = NQDomain.sover(25).solve

    assert(solution.isDefined)

    println("Found solution:")
    solution.get.print()
  }
}
