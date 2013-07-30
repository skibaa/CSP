package nqueens

import csp._
import csp.constraint.AllDifferentConstraint

object NQDomain {
  type TableType = Vector[Vector[Option[Boolean]]]

  case class NQVar(x:Int,dy:Int) extends Variable {
    def toNQVal(y:Int) = NQVal(y+(dy*x))
    def toY(nqval:NQVal) = nqval.y-(dy*x)
  }
  case class NQVal(y:Int) extends Value

  def empty(size:Int) = {
    val table = Vector.fill(size, size)(Option.empty[Boolean])
    new NQDomain(table)
  }

  def sover(size:Int) = {
    val empty = NQDomain.empty(size)
    val d = empty.remove(Map(
      empty.columns(0) -> (0 until empty.columns.size/2).map(i=>NQVal(i)).toSet,
      empty.columns(empty.columns.size - 1) -> Set(NQVal(0), NQVal(empty.size-1))
    )).get.asInstanceOf[NQDomain]

    val adc = new AllDifferentConstraint(d.columns.toSet)
    val adu = new AllDifferentConstraint(d.diagup.toSet)
    val add = new AllDifferentConstraint(d.diagdown.toSet)

    new Solver(d, Seq(adc, adu, add))
  }
}

class NQDomain private (table:NQDomain.TableType) extends Domain {
  import NQDomain._
  def size = table.size

  val columns = Vector.tabulate(size)(i => NQVar(i,0))
  val diagup = Vector.tabulate(size)(i => NQVar(i,1))
  val diagdown = Vector.tabulate(size)(i => NQVar(i,-1))

  override def print() = {
    table.foreach {row =>
      println (row.map(_ match {
        case None => "."
        case Some(true) => "X"
        case Some(false) => "o"
      }).mkString(""))
    }
  }
  lazy val contents: Domain.VarValMap = {
    def createMap(vars:Vector[NQVar])(x:Int) = {
      val xi = vars(x)
      val ysi = (0 until size).flatMap {
        y =>
          table(x)(y) match {
            case Some(false) => None
            case _ => Some(xi.toNQVal(y))
          }
      }
      if (ysi.isEmpty)
        None
      else
        Some((xi, ysi.toSet[Value]))
    }

    val columnsi = (0 until size).flatMap(createMap(columns)_)
    val diagupsi =  (0 until size).flatMap(createMap(diagup)_)
    val diagdownsi =  (0 until size).flatMap(createMap(diagdown)_)

    (columnsi ++ diagupsi ++ diagdownsi).toMap
  }

  private def updateTable(t:Vector[Vector[Option[Boolean]]], x:Int, y:Int, v:Option[Boolean]) = {
    t.updated(x, t(x).updated(y, v))
  }

  override def remove(x_v: Domain.VarValMap): Option[Domain] = {
    var newTable = table
    for {
      (nqvar@NQVar(x,dy), ys) <- x_v
      nqval@NQVal(_) <- ys
    } {
      val y = nqvar.toY(nqval)
      val oldCell = table(x)(y)
      newTable = updateTable(newTable, x, y, Some(false))
    }
    if (newTable == table)
      None
    else
      Some(new NQDomain(newTable))
  }

  override def bind(x_v:Map[Variable, Value]):Option[Domain] = {
    var newTable = table
    def unmark (x:Int, y:Int) = newTable(x)(y) match {
      case None =>
        newTable=updateTable(newTable, x, y, Some(false))
        true
      case Some(false) => true
      case Some(true) => false
    }
    val iter=x_v.iterator
    while(iter.hasNext) {
      iter.next() match {
        case(nqx @ NQVar(x,_), nqy @ NQVal(_)) =>
          val y = nqx.toY(nqy)
          if(table(x)(y) == Some(false))
            return None
          newTable = updateTable(newTable, x, y, Some(true))
          val d1 = x-y
          val d2 = x+y
          var i=0
          while(i < size) {
            if(i!=x)
              if(!unmark(i, y)) return None
            if(i!=y)
              if(!unmark(x, i)) return None
            val y1=i-d1
            if(i!=x && y1 >= 0 && y1 < size)
              if(!unmark(i, y1)) return None
            val y2=size-i-1
            val x2=d2-y
            if(x2!=x && x2 >= 0 && x2 < size)
              if(!unmark(x2, y2)) return None
            i += 1
          }
      }
    }

    if (newTable == table)
      Some(this)
    else
      Some(new NQDomain(newTable))
  }

  override def valWeight(xx:Variable)(v:Value):Option[Int] = (xx,v) match {
    case (nqx @ NQVar(x,dy), nqy @ NQVal(_)) =>
      if (dy != 0)
        return None //play only with first variable set for symmetry
      val y = nqx.toY(nqy)
      var res = Option(0)
      def weight(x:Int, y:Int) = table(x)(y) match {
        case None => Some(1)
        case Some(false) => Some(0)
        case Some(true) => None
      }
      val d1 = x-y
      val d2 = x+y
      var i=0
      while (i < size && res.isDefined) {
        if(i!=x)
          res = res.flatMap(r => weight(i, y).map(r +))
        if(i!=y)
          res = res.flatMap(r => weight(x, i).map(r +))
        val y1=i-d1
        if(i!=x && y1 >= 0 && y1 < size)
          res = res.flatMap(r => weight(i, y1).map(r +))
        val y2=size-i-1
        val x2=d2-y
        if(x2!=x && x2 >= 0 && x2 < size)
          res = res.flatMap(r => weight(x2, y2).map(r +))
        i += 1
      }
      res
    case _ => throw new IllegalArgumentException
  }

}
