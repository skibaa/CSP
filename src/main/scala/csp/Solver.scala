package csp

import csp.constraint.Constraint

object Solver {
  var badDomains = collection.mutable.Set.empty[Int]

  def addBadDomain(bad:Int) {
    if (badDomains.size > 10000)
      badDomains = collection.mutable.Set.empty[Int]
    badDomains += bad
  }
}

class Solver(domain:Domain, constraints:Seq[Constraint]) {
  def isSolved =
    domain.contents.forall { case (x, vals) =>
      vals.size == 1
    }

  def solve:Option[Domain] = {
    if (Solver.badDomains.contains(domain.hashCode())) {
      println("pruning bad domain")
      return None
    }

    if (isSolved)
      return Some(domain)

//    println("Trying to solve:")
//    domain.print

    var boundDomain = domain

    val definedVars = domain.contents.flatMap {
      case (x,vals) if vals.size==1 => Some(x,vals.head)
      case _ => None
    }

    val iter = definedVars.iterator
    while (iter.hasNext) {
      val (x,v) = iter.next()
      tryBind(boundDomain, x, v) match {
        case None => return None
        case Some(newDomain) => boundDomain = newDomain
      }
    }

    val filteredVars = domain.contents.filter {case (x, _) => !definedVars.contains(x)}

    val sortedPairs = filteredVars.flatMap { case (x, vals) =>
      vals.map(v => (x,v))
    }.map{case (x, v) => (boundDomain.valWeight(x)(v), x, v)}.
      filter(_._1.isDefined).toSeq.sortBy(_._1).map(t => (t._2, t._3))

    //using iterator to be able to return from the middle
    val varIterator = sortedPairs.iterator

    while (varIterator.hasNext) {
      val (x, v) = varIterator.next()
      val tryResult = tryBindAndSolve(boundDomain, x, v)
      if (tryResult.isDefined)
        return tryResult
    }

    Solver.addBadDomain(domain.hashCode)

    None
  }

  private def tryBindAndSolve(tryDomain:Domain, x:Variable, v:Value):Option[Domain] =
    tryBind(tryDomain, x, v) match {
      case None => None
      case Some(newDomain) =>
        new Solver(newDomain, constraints).solve
    }

  private def tryBind(tryDomain: Domain, x: Variable, v: Value): Option[Domain] = {
    val maybeBoundDomain = tryDomain.bind(Map(x -> v))
    if (maybeBoundDomain.isEmpty)
      return None

    var newDomain = maybeBoundDomain.get
    var oldDomain = newDomain

    do {
      oldDomain = newDomain
      val checkResults = constraints.map(_ check newDomain)

      if (!checkResults.forall(_.isFeasible))
        return None

      checkResults.foreach {
        checkResult =>
          val mayBePruned = newDomain.remove(checkResult.prune)
          newDomain = mayBePruned.getOrElse(newDomain)
      }
    } while (newDomain != oldDomain)
    Some(newDomain)
  }
}
