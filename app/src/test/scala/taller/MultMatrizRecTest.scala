package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import App._ 

@RunWith(classOf[JUnitRunner])
class MultMatrizRecTest extends AnyFunSuite {

    test("Multiplicación recursiva de matriz 2x2 y 2x2") {
    val m1: Matriz = Vector(
        Vector(1, 2),
        Vector(3, 4)
    )
    val m2: Matriz = Vector(
        Vector(5, 6), 
        Vector(7, 8)
    )
    val resultadoEsperado: Matriz = Vector(
        Vector(19, 22), 
        Vector(43, 50)  
    )
    assert(multMatrizRec(m1, m2) == resultadoEsperado)
    }

    test("Multiplicación recursiva de matriz 4x4 y 4x4") {
    val m1: Matriz = Vector(
        Vector(1, 2, 3, 4),
        Vector(5, 6, 7, 8),
        Vector(9, 10, 11, 12),
        Vector(13, 14, 15, 16)
    )
    val m2: Matriz = Vector(
        Vector(17, 18, 19, 20),
        Vector(21, 22, 23, 24),
        Vector(25, 26, 27, 28),
        Vector(29, 30, 31, 32)
    )
    val resultadoEsperado: Matriz = Vector(
        Vector(250, 260, 270, 280),
        Vector(618, 644, 670, 696),
        Vector(986, 1028, 1070, 1112),
        Vector(1354, 1412, 1470, 1528)
    )
    assert(multMatrizRec(m1, m2) == resultadoEsperado)
    }

    test("Multiplicación recursiva de matriz identidad 2x2") {
    val m1: Matriz = Vector(
        Vector(1, 2),
        Vector(3, 4)
    )
    val identidad: Matriz = Vector(
        Vector(1, 0),
        Vector(0, 1)
    )
    val resultadoEsperado: Matriz = m1
    assert(multMatrizRec(m1, identidad) == resultadoEsperado)
    }

    test("Multiplicación recursiva con matriz de ceros 3x3") {
    val m1: Matriz = Vector(
        Vector(1, 2, 3),
        Vector(4, 5, 6),
        Vector(7, 8, 9)
    )
    val ceros: Matriz = Vector(
        Vector(0, 0, 0),
        Vector(0, 0, 0),
        Vector(0, 0, 0)
    )

    assert(multMatrizRec(m1, ceros) == Vector())
    }


}