package taller
import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestMatrices extends AnyFunSuite {

    test("matrizAlAzar(3, 3)") {
        val matriz = App.matrizAlAzar(3, 3)
        assert(matriz.length == 3)
        assert(matriz.forall(_.length == 3)) // Forall que recorre cada fila de la matriz
    }

    test("vectorAlAzar(3, 3)") {
        val vector = App.vectorAlAzar(3, 3)
        assert(vector.length == 3)
    }

    test("multMatrizRec test") {
        val m1 = App.matrizAlAzar(8, 8)
        val m2 = App.matrizAlAzar(8, 8)
        println("Matriz 1:")
        App.printMatriz(m1)
        println("Matriz 2:")
        App.printMatriz(m2)
        val m3 = App.multMatrizRec(m1, m2)
        println("Resultado Matriz:")
        App.printMatriz(m3)
        assert(m3.length == 8)
        assert(m3.forall(_.length == 8))
    }
}
