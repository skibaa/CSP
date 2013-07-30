package csp

import org.scalatest.FunSuite

class SimpleConstraintsSuite extends FunSuite {
  object x extends Variable
  object y extends Variable
  object z extends Variable
  object one extends Value
  object two extends Value
  object three extends Value
  object four extends Value

  test("test simple constraints") {
    import constraint._
    import Implicits._

    val c1 = x in Set(one, two, three)
    val c2 = ! (x in Set(three, four))
    val c3 = c1 && c2

    val d = Domain(Map(x -> Set(two, three)))

    val ch1 = c1.check(d)
    val ch3 = c3.check(d)

    val d1 = ch1.prune
    val d3 = ch3.prune

    assert(d1.isEmpty)
    assert(d3 === Map(x->Set(three)))
  }
}
