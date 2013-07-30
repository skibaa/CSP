package csp.constraint

import csp._
import Domain._
import csp.constraint.Constraint.CheckResult

object Constraint {
  trait CheckResult {
    def isFeasible:Boolean
    def prune:VarValMap
  }

  def infeasible = new CheckResult {
    def isFeasible: Boolean = false

    def prune: Domain.VarValMap = throw new UnsupportedOperationException
  }
}

trait Constraint {

  def check(d:Domain):CheckResult
}
