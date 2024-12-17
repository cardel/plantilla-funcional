package taller

import scala.util.Random
import common._
import org.scalameter.withWarmer
import org.scalameter.Warmer
import org.scalameter.measure
import org.scalameter.Warmer.Default
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._
import org.scalameter._
import org.scalatest.funsuite.AnyFunSuite

class RiegoOptimoTest extends AnyFunSuite {

  // Instancia de la clase RiegoOptimo
  val riegoOptimo = new RiegoOptimo()

  test("tIR calcula correctamente los tiempos de inicio de riego") {
    // Definimos una finca de prueba
    val finca: riegoOptimo.Finca = Vector(
      (8, 3, 1), // Tablón 0: tiempo_supervivencia=8, tiempo_riego=3, prioridad=1
      (5, 2, 2), // Tablón 1: tiempo_supervivencia=5, tiempo_riego=2, prioridad=2
      (7, 4, 1)  // Tablón 2: tiempo_supervivencia=7, tiempo_riego=4, prioridad=1
    )

    // Definimos la programación de riego
    val progRiego: riegoOptimo.ProgRiego = Vector(0, 2, 1) // Primero riega el tablón 0, luego el 2, y finalmente el 1

    // Calculamos los tiempos de inicio de riego
    val tiemposInicioRiego: riegoOptimo.TiempoInicioRiego = riegoOptimo.tIR(finca, progRiego)

    // Resultado esperado
    val expected: Vector[Int] = Vector(0, 7, 3)

    // Validamos que el resultado sea el esperado
    assert(tiemposInicioRiego == expected)
  }

  test("tIR con un solo tablón debe devolver un inicio de riego en 0") {
    // Finca con un solo tablón
    val finca: riegoOptimo.Finca = Vector(
      (10, 5, 1) // Tablón 0: tiempo_supervivencia=10, tiempo_riego=5, prioridad=1
    )

    // Programación de riego para un solo tablón
    val progRiego: riegoOptimo.ProgRiego = Vector(0)

    // Calculamos los tiempos de inicio de riego
    val tiemposInicioRiego: riegoOptimo.TiempoInicioRiego = riegoOptimo.tIR(finca, progRiego)

    // Resultado esperado
    val expected: Vector[Int] = Vector(0)

    // Validamos que el resultado sea el esperado
    assert(tiemposInicioRiego == expected)
  }

  test("Costo de riego: Tablón regado a tiempo") {
    // Definir finca
    val finca = Vector(
      (10, 2, 1), // Tablón 0: tsup=10, treg=2, prio=1
      (8, 3, 2),  // Tablón 1: tsup=8, treg=3, prio=2
      (6, 1, 3),  // Tablón 2: tsup=6, treg=1, prio=3
      (7, 4, 1)   // Tablón 3: tsup=7, treg=4, prio=1
    )

    // Programación de riego
    val progRiego = Vector(0, 3, 1, 2)

    // Cálculo del costo para el Tablón 3 (regado a tiempo)
    val costo = riegoOptimo.costoRiegoTablon(3, finca, progRiego)
    assert(costo == 1) // El tiempo restante es 1
  }

  test("Costo de riego: Tablón regado tarde") {
    // Definir finca
    val finca = Vector(
      (10, 2, 1), // Tablón 0: tsup=10, treg=2, prio=1
      (8, 3, 2),  // Tablón 1: tsup=8, treg=3, prio=2
      (6, 1, 3),  // Tablón 2: tsup=6, treg=1, prio=3
      (7, 4, 1)   // Tablón 3: tsup=7, treg=4, prio=1
    )

    // Programación de riego
    val progRiego = Vector(0, 3, 1, 2)

    // Cálculo del costo para el Tablón 1 (regado tarde)
    val costo = riegoOptimo.costoRiegoTablon(1, finca, progRiego)
    assert(costo == 2) // Penalización: prio * tiempo excedido = 2 * 1 = 2
  }

  test("Costo de riego para toda la finca") {
    // Definir finca
    val finca = Vector(
      (10, 2, 1), // Tablón 0: tsup=10, treg=2, prio=1
      (8, 3, 2),  // Tablón 1: tsup=8, treg=3, prio=2
      (6, 1, 3),  // Tablón 2: tsup=6, treg=1, prio=3
      (7, 4, 1)   // Tablón 3: tsup=7, treg=4, prio=1
    )

    // Programación de riego
    val progRiego = Vector(0, 3, 1, 2)

    // Cálculo del costo total para toda la finca
    val costoTotal = riegoOptimo.costoRiegoFinca(finca, progRiego)

    // El valor esperado del costo total para la finca (calculado previamente o dado por el problema)
    val costoEsperado = 7 // Este es el valor esperado para el costo total.

    // Comprobar que el costo total calculado coincide con el costo esperado
    assert(costoTotal == costoEsperado)
  }

  test("Costo de movilidad para el programa de riego") {
    // Definir finca (tiempo supervivencia, tiempo riego, prioridad)
    val finca = Vector(
      (10, 2, 1), // Tablón 0
      (8, 3, 2),  // Tablón 1
      (6, 1, 3)   // Tablón 2
    )

    // Definir el programa de riego: el orden en el que se riegan los tablones
    val progRiego = Vector(0, 2, 1)  // El orden es: Tablón 0, Tablón 2, Tablón 1

    // Definir la matriz de distancias entre los tablones
    val distancias = Vector(
      Vector(0, 5, 8),  // Distancias desde el Tablón 0
      Vector(5, 0, 3),  // Distancias desde el Tablón 1
      Vector(8, 3, 0)   // Distancias desde el Tablón 2
    )

    // Calcular el costo de movilidad utilizando la función
    val costoTotalMovilidad = riegoOptimo.costoMovilidad(finca, progRiego, distancias)

    // Valor esperado del costo de movilidad
    val costoEsperado = 8 + 3  // Distancia entre el Tablón 0 y Tablón 2 (8), y entre Tablón 2 y Tablón 1 (3)

    // Validar que el costo calculado es el esperado
    assert(costoTotalMovilidad == costoEsperado, s"Se esperaba $costoEsperado pero se obtuvo $costoTotalMovilidad")
  } 
  test("ProgramacionRiegoOptimo debería devolver la programación y el costo óptimos") {
    val riegoOptimo = new RiegoOptimo

    // Datos de entrada
    val finca: riegoOptimo.Finca = Vector(
      (10, 3, 2), // Tablón 0: (supervivencia = 10, riego = 3, prioridad = 2)
      (8, 2, 1),  // Tablón 1: (supervivencia = 8, riego = 2, prioridad = 1)
      (12, 1, 3)  // Tablón 2: (supervivencia = 12, riego = 1, prioridad = 3)
    )

    val distancias: riegoOptimo.Distancia = Vector(
      Vector(0, 4, 5), // Distancias desde Tablón 0
      Vector(4, 0, 3), // Distancias desde Tablón 1
      Vector(5, 3, 0)  // Distancias desde Tablón 2
    )

    // Valores esperados
    val programacionEsperada: riegoOptimo.ProgRiego = Vector(0, 1, 2) // Óptimo
    val costoEsperado = 31 // Suma del costo de riego y movilidad

    // Llamar a la función ProgramacionRiegoOptimo
    val (programacionOptima, costoOptimo) = riegoOptimo.ProgramacionRiegoOptimo(finca, distancias)

    // Validar que los resultados sean los esperados
    assert(programacionOptima == programacionEsperada, "La programación de riego óptima no es la esperada")
    assert(costoOptimo == costoEsperado, "El costo total óptimo no es el esperado")
  }


  test("costoRiegoFincaPar debe devolver el mismo resultado que costoRiegoFinca") {
    val finca: riegoOptimo.Finca = Vector(
      (10, 2, 1), // Tablón 0
      (8, 3, 2),  // Tablón 1
      (6, 1, 3)   // Tablón 2
    )

    val progRiego: riegoOptimo.ProgRiego = Vector(0, 2, 1)

    val costoSeq = riegoOptimo.costoRiegoFinca(finca, progRiego)
    val costoPar = riegoOptimo.costoRiegoFincaPar(finca, progRiego)

    assert(costoSeq == costoPar, s"Los costos deben coincidir, pero se obtuvo secuencial: $costoSeq, paralelo: $costoPar")
  }

  test("costoMovilidadPar debe devolver el mismo resultado que costoMovilidad") {
    val finca: riegoOptimo.Finca = Vector(
      (10, 2, 1), // Tablón 0
      (8, 3, 2),  // Tablón 1
      (6, 1, 3)   // Tablón 2
    )

    val progRiego: riegoOptimo.ProgRiego = Vector(0, 2, 1)

    val distancias: riegoOptimo.Distancia = Vector(
      Vector(0, 4, 5), // Distancias desde el Tablón 0
      Vector(4, 0, 3), // Distancias desde el Tablón 1
      Vector(5, 3, 0)  // Distancias desde el Tablón 2
    )

    val costoSeq = riegoOptimo.costoMovilidad(finca, progRiego, distancias)
    val costoPar = riegoOptimo.costoMovilidadPar(finca, progRiego, distancias)

    assert(costoSeq == costoPar, s"Los costos deben coincidir, pero se obtuvo secuencial: $costoSeq, paralelo: $costoPar")
  }

  test("generarProgramacionesRiegoPar debe devolver el mismo resultado que generarProgramacionesRiego") {
    val finca: riegoOptimo.Finca = Vector(
      (10, 2, 1), // Tablón 0
      (8, 3, 2),  // Tablón 1
      (6, 1, 3)   // Tablón 2
    )

    val progSeq = riegoOptimo.generarProgramacionesRiego(finca)
    val progPar = riegoOptimo.generarProgramacionesRiegoPar(finca)

    assert(progSeq == progPar, "Las programaciones generadas deben coincidir entre secuencial y paralelo")
  }

  test("ProgramacionRiegoOptimoPar debe devolver el mismo resultado que ProgramacionRiegoOptimo") {
    val finca: riegoOptimo.Finca = Vector(
      (10, 2, 1), // Tablón 0
      (8, 3, 2),  // Tablón 1
      (6, 1, 3)   // Tablón 2
    )

    val distancias: riegoOptimo.Distancia = Vector(
      Vector(0, 4, 5), // Distancias desde el Tablón 0
      Vector(4, 0, 3), // Distancias desde el Tablón 1
      Vector(5, 3, 0)  // Distancias desde el Tablón 2
    )

    val (progSeq, costoSeq) = riegoOptimo.ProgramacionRiegoOptimo(finca, distancias)
    val (progPar, costoPar) = riegoOptimo.ProgramacionRiegoOptimoPar(finca, distancias)

    assert(progSeq == progPar, "La programación óptima debe coincidir entre secuencial y paralelo")
    assert(costoSeq == costoPar, "El costo óptimo debe coincidir entre secuencial y paralelo")
  }
}




