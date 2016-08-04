package catalog

import org.scalatest.FunSuite

class CatalogSuite extends FunSuite {
  val a = new Item("A", 1)
  val b = new Item("B", 2)
  val c = new Item("C", 3)
  val d = new Item("D", 4)
  val its = Set((a, BigDecimal(12.50)), (b, BigDecimal(1.50)), (c, BigDecimal(5.00)), (d, BigDecimal(30.00)))
  val b0 = (Set((a, 1), (c, 1)), BigDecimal(15.00))
  val b1 = (Set((b, 1), (c, 2)), BigDecimal(11.00))
  val b2 = (Set((d, 3)), BigDecimal(60.00))
  val buns = Set(b0, b1, b2)
  val cat = Catalog(its, buns)
  val pf0 = () => cat.price(Set((a, 1), (b, 2)))
  val r0 = BigDecimal(15.50)
  val pf1 = () => cat.price(Set((a, 1), (b, 2), (c, 2)))
  val r1 = BigDecimal(23.00)
  val pf2 = () => cat.price(Set((a, 1), (b, 2), (c, 3), (d, 2)))
  val r2 = BigDecimal(87.50)
  val pf3 = () => cat.price(Set((a, 1), (b, 1), (c, 3)))
  val r3 = BigDecimal(26.00)
  val pf4 = () => cat.price(Set((d, 3)))
  val r4 = BigDecimal(60.00)
  val pf5 = () => cat.price(Set((a, 2), (b, 2), (c, 2)))
  val r5 = BigDecimal(33.00)

  test("no bundle in cart") {
    assert(pf0() === r0)
  }

  test("buy 2, get 1 free") {
    assert(pf4() === r4)
  }

  test("item in multiple eligible bundles") {
    assert(pf1() === r1)
  }

  test("multiple bundles in cart") {
    assert(pf2() === r2)
  }

  test("multiple bundles in cart w/ no leftover items") {
    assert(pf3() === r3)
  }
  
  test("bundle applied multiple times to cart") {
    assert(pf5() === r5)
  }

  test("concurrent price requests") {
    import scala.concurrent._
    import scala.concurrent.duration._
    import java.util.concurrent.Executors.newFixedThreadPool

    val funResults = Set((pf0, r0), (pf1, r1), (pf2, r2), (pf3, r3), (pf4, r4), (pf5, r5))

    implicit val ec = ExecutionContext fromExecutor newFixedThreadPool(funResults.size)

    funResults.foreach {
      case (func, result) =>
        assert(Await.result(Future { func() }, 1 second) === result)
    }
  }

  test("large # of bundles") {
    val bigBuns = Vector.tabulate(10000) { i =>
      (Set((a, i + 1), (c, 1)), BigDecimal((15.00 * i) - 1))
    } ++ Vector.tabulate(10000) { i =>
      (Set((b, 1), (c, 2 + i)), BigDecimal((11.00 - 1) * i))
    } ++ Vector.tabulate(10000) { i =>
      (Set((d, 10001 - i)), BigDecimal(10010.00 - i))
    }
    val cat1 = Catalog(its, bigBuns.toSet)
    val p = cat1.price(Set((a, 3), (b, 1), (c, 2), (d, 5)))
    assert(p === BigDecimal(26.00))
  }
}