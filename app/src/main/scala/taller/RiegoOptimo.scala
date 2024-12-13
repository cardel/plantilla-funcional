package taller

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import org.scalameter._

class RiegoOptimo {

  // DATOS
      type Tablon = (Int, Int, Int)
      /* Un tablon es una tripleta con el tiempo de supervivencia,
      el tiempo de riego y la prioridad del tablon
      */

      type Finca = Vector [ Tablon ]
      /* Una finca es un vector de tablones
      Si f : Finca , f(i) = (tsi, tri, pi)
      */

      type Distancia = Vector [ Vector [ Int ] ]
      /* La distancia entre dos tablones se representa por
       una matriz
       */

      type ProgRiego = Vector [ Int ]
      /* Una programación de riego es un vector que asocia
      cada tablon i con su turno de riego (0 es el primer turno,
      n-1 es el último turno)

      Si v : ProgRiego , y v.length == n, v es una permutaci´on
      de {0, ..., n-1} v(i) es el turno de riego del tablon i
      para 0 <= i < n
      */



      type TiempoInicioRiego = Vector [ Int ]
      /* El tiempo de inicio de riego es un vector que asocia
      cada tablon i con el momento del tiempo en que se riega
      Si t : TiempoInicioRiego y t.length == n, t(i) es la hora a
      la que inicia a regarse el tablon i
      */

  // GENERACIONES

      val random = new Random( )
      def fincaAlAzar (long: Int): Finca = {

      /* Crea una finca de long tablones ,
      con valores aleatorios entre 1 y long * 2 para el tiempo
      de supervivencia , entre 1 y long para el tiempo
      de regado y entre 1 y 4 para la prioridad
      */
      val v = Vector.fill(long)(
        (random.nextInt(long * 2) + 1,
        random.nextInt(long) + 1,
        random.nextInt(4) + 1)
      )
    v
    }

      def distanciaAlAzar(long: Int): Distancia = {

        /* Crea una matriz de distancias para una finca
        de long tablones , con valores aleatorios entre
        1 y long * 3
        */

      val v = Vector.fill(long, long)(random.nextInt(long * 3) + 1)
      Vector.tabulate(long, long)((i, j) =>
        if (i < j) v(i)(j)
        else if (i == j) 0
        else v (j)(i))
    }

  // ENTRADAS

      def tsup(f: Finca, i: Int): Int = {
        f(i)._1
    }

      def treg(f: Finca, i: Int): Int = {
        f(i)._2
    }

      def prio(f: Finca, i: Int): Int = {
        f(i)._3
    }

  // CALCULO TIEMPO INICIO RIEGO

      def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
        /* Dada una finca f y una programación de riego pi,
        y f.length == n, tIR(f, pi) devuelve t: TiempoInicioRiego
        tal que t(i) es el tiempo en que inicia el riego del
        tablon i de la finca f según pi
        */

      val tiempos = Array.fill(f.length)(0)
      for (j <- 1 until pi.length) {
        val prevTablon = pi(j − 1)
        val currTablon = pi(j)
        tiempos(currTablon) = tiempos(prevTablon) + treg(f, prevTablon)
      }
      tiempos.toVector
    }

  // CALCULO COSTOS

      def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
        val tiempoInicio = tIR(f, pi)(i)
        val tiempoFinal = tiempoInicio + treg(f, i)
          if (tsup(f, i) - treg(f, i) >= tiempoInicio){
            tsup(f, i) - tiempoFinal
          } else {
            prio(f, i) * (tiempoFinal - tsup(f, i))
          }
      }

      def costoRiegoFinca(f: Finca, pi: ProgRiego): Int = {
        (0 until f.length).map( i => costoRiegoTablon(i, f, pi)).sum
      }

      def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
        (0 until pi.length - 1).map(j => d(pi(j)) (pi(j + 1))).sum
      }

  // GENERACIONES DE PROGRAMACIÓN DE RIEGO

  def generarProgramacionesRiego(f: Finca): Vector[ProgRiego] = {

    /* Dada una finca de n tablones , devuelve todas las
    posibles programaciones de riego de la finca
    */

    val indices = (0 until f.length).toVector
    indices.permutations.toVector
  }

  // CALCULO PROGRAMACIÓN RIEGO OPTIMA

  def ProgramacionRiegoOptimo(f: Finca, d: Distancia) : (ProgRiego, Int) = {
    // Dada una finca devuelve la programación
    // de riego óptima
    val programaciones = generarProgramacionesRiego(f)
    val costos = programaciones.map( pi =>
      (pi, costoRiegoFinca (f, pi) + costoMovilidad(f, pi, d))
    )
    costos.minBy(_._2)
  }

  // ACELERANDO CALCULOS CON PARALELISMO DE TAREAS Y DATOS

  def costoRiegoFincaPar(f: Finca, pi: ProgRiego): Int = {
    // Devuelve el costo total de regar una finca f dada una
    // programaciÓn de riego pi, calculando en paralelo
    (0 until f.length).par.map(i => costoRiegoTablon (i, f, pi)).sum
  }

  def costoMovilidadPar(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    // Calcula el costo de movilidad de manera paralela
    (0 until pi.length - 1).par.map(j => d(pi(j)) (pi(j + 1))).sum
  }

  // PARALELIZAR LA GENERACIÓN DE PROGRAMACIONES DE RIEGO

  def generarProgramacionesRiegoPar(f: Finca): Vector[ProgRiego] = {
    // Genera las programaciones posibles de manera paralela
    val indices = (0 until f.length).toVector
    indices.permutations.toVector.par.toVector
  }

  // PARALELIZAR LA GENERACIÓN DE PROGRAMACIONES DE RIEGO OPTIMA

  def ProgramacionRiegoOptimoPar (f: Finca, d: Distancia): (ProgRiego, Int) = {
    // Dada una finca, calcula la programación óptima de riego
    val programaciones = generarProgramacionesRiegoPar(f)
    val costos = programaciones.par.map(pi => (pi, costoRiegoFincaPar(f, pi) + costoMovilidadPar(f, pi, d))
    )
    costos.minBy(_._2)
  }

  // GENERACION DE DATOS

  val timeSeq = measure {
    ProgramacionRiegoOptimo(finca, distancia)
  }
  val timePar = measure {
    ProgramacionRiegoOptimoPar (finca, distancia)
  }
  println(s"Secuencial: $timeSeq ms")
  println(s"Paralelo: $timePar ms")
}
