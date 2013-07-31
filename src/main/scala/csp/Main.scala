package csp

import nqueens.NQDomain
import nqueens.NQDomain.{NQVal, NQVar}

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    val size = args(0).toInt

    val solver = NQDomain.solver(size)
    val solution = solver.solve.get

    println(size)

    val ys = (0 until size).map { i =>
      val x = NQVar(i, 0)
      val vals = solution.contents(x)
      assert(vals.size == 1)
      val y = vals.head.asInstanceOf[NQVal].y
      y
    }

    println(ys.mkString(" "))
  }

}
