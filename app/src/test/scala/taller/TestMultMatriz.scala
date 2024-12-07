package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MultMatrizTest extends AnyFunSuite {
    
    val multMatriz = new MultMatriz()
    
    test("Multiplicación de matrices 1x1") {
        val m1 = Vector(Vector(2))
        val m2 = Vector(Vector(3))
        val expected = Vector(Vector(6))
        assert(multMatriz.multMatrizRec(m1, m2) == expected)
    }
    
    test("Multiplicación de matrices 2x2") {
        val m1 = Vector(
            Vector(1, 2),
            Vector(3, 4)
        )
        val m2 = Vector(
            Vector(5, 6),
            Vector(7, 8)
        )
        val expected = Vector(
            Vector(19, 22),
            Vector(43, 50)
        )
        assert(multMatriz.multMatrizRec(m1, m2) == expected)
    }
    
    test("Multiplicación de matrices 4x4") {
        val m1 = Vector(
            Vector(1, 2, 3, 4),
            Vector(5, 6, 7, 8),
            Vector(9, 10, 11, 12),
            Vector(13, 14, 15, 16)
        )
        val m2 = Vector(
            Vector(1, 2, 3, 4),
            Vector(5, 6, 7, 8),
            Vector(9, 10, 11, 12),
            Vector(13, 14, 15, 16)
        )
        val expected = Vector(
            Vector(90, 100, 110, 120),
            Vector(202, 228, 254, 280),
            Vector(314, 356, 398, 440),
            Vector(426, 484, 542, 600)
        )
        assert(multMatriz.multMatrizRec(m1, m2) == expected)
    }
    
    test("Multiplicación por matriz identidad 2x2") {
        val m1 = Vector(
            Vector(1, 2),
            Vector(3, 4)
        )
        val identidad = Vector(
            Vector(1, 0),
            Vector(0, 1)
        )
        assert(multMatriz.multMatrizRec(m1, identidad) == m1)
    }
    
    test("Multiplicación por matriz de ceros 2x2") {
        val m1 = Vector(
            Vector(1, 2),
            Vector(3, 4)
        )
        val ceros = Vector(
            Vector(0, 0),
            Vector(0, 0)
        )
        val expected = Vector(
            Vector(0, 0),
            Vector(0, 0)
        )
        assert(multMatriz.multMatrizRec(m1, ceros) == expected)
    }
}