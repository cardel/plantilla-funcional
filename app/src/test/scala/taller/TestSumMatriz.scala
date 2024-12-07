package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSumMatriz extends AnyFunSuite {
    val obj = new SumMatriz()

    val m1 = Vector(
        Vector(1, 2),
        Vector(3, 4)
    )
    val m2 = Vector(
        Vector(5, 6),
        Vector(7, 8)
    )

    val result = obj.sumMatriz(m1, m2)

    test("SumMatriz Test #1") {
        assert(result(0)(0) == 6)
    }

    test("SumMatriz Test #2") {
        assert(result(0)(1) == 8)
    }

    test("SumMatriz Test #3") {
        assert(result(1)(0) == 10)
    }

    test("SumMatriz Test #4") {
        assert(result(1)(1) == 12)  
    }

    test("SumMatriz Test #5") {
        assert(result.length == m1.length)
        assert(result(0).length == m1(0).length)
    }
}