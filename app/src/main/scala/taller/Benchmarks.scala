package taller

import scala.collection.parallel.immutable._
import org.scalameter._
import taller.App.{prodPunto, prodPuntoParD}
import scala.collection.parallel.CollectionConverters._

object Benchmarks {
  //Benchmarking para el producto punto
  def benchmarkingVectores():Unit = {

    val size = 100 // Tamaño del vector

    println(s"Benchmarking para vectores de tamaño $size:")

    for (i <- 1 to 10) {
      // Generar vectores aleatorios
      val v1 = App.vectorAlAzar(size, 100)
      val v2 = App.vectorAlAzar(size, 100)
      val parV1: ParVector[Int] = v1.par
      val parV2: ParVector[Int] = v2.par

      val seqTime = withWarmer(new Warmer.Default) measure {
        prodPunto(v1, v2)
      }

      val parTime = withWarmer(new Warmer.Default) measure {
        prodPuntoParD(parV1, parV2)
      }

      // Imprimir resultados
      println(f"Iteración $i:")
      println(f"  Secuencial: $seqTime")
      println(f"  Paralelo:   $parTime")
    }
  }

  //Benchmarking para la multiplicación de matrices
  def benchmarkingMultMatrizRec():Unit = {

    println(s"Benchmarking para matrices :")

    for (i <- 1 to 7) {
      // Generar matrices aleatorias con limite de 2^i
      // Se generan matrices de 2^i x 2

      val m1 = App.matrizAlAzar(math.pow(2 , i ) . toInt , 2)
      val m2 = App.matrizAlAzar(math.pow(2 , i ) . toInt , 2)
      val parM1: App.Matriz = m1.par.map(_.toVector).toVector
      val parM2: App.Matriz = m2.par.map(_.toVector).toVector

      val seqTime = withWarmer(new Warmer.Default) measure {
        App.multMatrizRec(m1, m2)
      }

      val parTime = withWarmer(new Warmer.Default) measure {
        App.multMatrizRecPar(parM1, parM2)
      }

      // Imprimir resultados
      println(f"Iteración $i:")
      println(f"  Secuencial: $seqTime")
      println(f"  Paralelo: $parTime")
    }
  }

  //Benchmarking para la multiplicación de matrices con metodo Strassen
  def benchmarkingMultMatrizStrassen():Unit = {

    println(s"Benchmarking para matrices con metodo Strassen:")

    for (i <- 1 to 6) { // hasta 6 por temas de memoria :c
      // Generar matrices aleatorias con limite de 2^i
      // Se generan matrices de 2^i x 2

      val m1 = App.matrizAlAzar(math.pow(2 , i ) . toInt , 2)
      val m2 = App.matrizAlAzar(math.pow(2 , i ) . toInt , 2)
      val parM1: App.Matriz = m1.par.map(_.toVector).toVector
      val parM2: App.Matriz = m2.par.map(_.toVector).toVector

      val seqTime = withWarmer(new Warmer.Default) measure {
        App.multStrassen(m1, m2)
      }

      val parTime = withWarmer(new Warmer.Default) measure {
        App.multStrassenPar(parM1, parM2)
      }

      // Imprimir resultados
      println(f"Iteración $i:")
      println(f"  Secuencial: $seqTime")
      println(f"  Paralelo: $parTime")
      println("Resultado Matriz:")
      App.printMatriz(App.multMatrizRec(m1, m2))
    }
  }
}
