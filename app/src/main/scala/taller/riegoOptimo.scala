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

class RiegoOptimo {
  type Tablon = (Int, Int, Int) // tiempo supervivencia, tiempo riego, prioridad tablon
  type Finca = Vector[Tablon]  
  type Distancia = Vector[Vector[Int]] // matriz de distancias
  type ProgRiego = Vector[Int]
  type TiempoInicioRiego = Vector[Int]

  val random = new Random()

  // Generación de finca al azar
  def fincaAlAzar(long: Int): Finca = {
    val v = Vector.fill(long)((random.nextInt(long * 2) + 1, random.nextInt(long) + 1, random.nextInt(4) + 1))
    v
  }

  // Generación de matriz de distancias al azar
  def distanciaAlAzar(long: Int): Distancia = {
    val v = Vector.fill(long, long)(random.nextInt(long * 3) + 1)
    Vector.tabulate(long, long)((i, j) => if (i < j) v(i)(j) else if (i == j) 0 else v(j)(i))
  }

  // Funciones de acceso a los valores de finca
  def tsup(f: Finca, i: Int): Int = f(i)._1
  def treg(f: Finca, i: Int): Int = f(i)._2
  def prio(f: Finca, i: Int): Int = f(i)._3

  // Cálculo de tiempos de inicio de riego
  def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
    val tiempos = Array.fill(f.length)(0)
    for (j <- 1 until pi.length) {
      val prevTablon = pi(j - 1)
      val currtablon = pi(j)
      tiempos(currtablon) = tiempos(prevTablon) + treg(f, prevTablon)
    }
    tiempos.toVector
  }

  // Cálculo del costo de riego de un tablón
  def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
    val tiempoInicio = tIR(f, pi)(i)
    val tiempoFinal = tiempoInicio + treg(f, i)
    if (tsup(f, i) - treg(f, i) >= tiempoInicio) {
      tsup(f, i) - tiempoFinal
    } else {
      prio(f, i) * (tiempoFinal - tsup(f, i))
    }
  }
  
  def costoRiegoFinca(f: Finca, pi: ProgRiego): Int = {
    (0 until f.length).map(i => costoRiegoTablon(i, f, pi)).sum
  }


  def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    (0 until pi.length - 1).map(j => d(pi(j))(pi(j + 1))).sum
  }

  
  def generarProgramacionesRiego(f: Finca): Vector[ProgRiego] = {
    val indices = (0 until f.length).toVector
    indices.permutations.toVector
  }

 
  def ProgramacionRiegoOptimo(f: Finca, d: Distancia): (ProgRiego, Int) = {
    val programaciones = generarProgramacionesRiego(f)
    val costos = programaciones.map(pi =>
      (pi, costoRiegoFinca(f, pi) + costoMovilidad(f, pi, d))
    )
    costos.minBy(_._2) // Seleccionar la programación con el costo mínimo
  }

  def costoRiegoFincaPar(f: Finca, pi:ProgRiego): Int = {
    (0 until f.length).par.map(i => costoRiegoTablon(i, f, pi)).sum
  }

  def costoMovilidadPar(f:Finca,pi:ProgRiego, d:Distancia): Int = {
    (0 until pi.length - 1).par.map(j => d(pi(j))(pi(j + 1))).sum
  }

  def generarProgramacionesRiegoPar(f:Finca): Vector[ProgRiego] = {
    val indices = (0 until f.length).toVector
    indices.permutations.toVector.par.toVector
  }

  def ProgramacionRiegoOptimoPar(f:Finca, d:Distancia): (ProgRiego, Int) = {
    val programaciones = generarProgramacionesRiegoPar(f)
    val costos = programaciones.par.map(pi =>
      (pi, costoRiegoFincaPar(f, pi) + costoMovilidadPar(f, pi, d))
    )
    costos.minBy(_._2)
  }


  def compararCostoRiego(seq: (Finca, ProgRiego)=> Int, par: (Finca, ProgRiego)=> Int)(finca: Finca, programariego: ProgRiego): List[Double] = {
    val seqTime = withWarmer(new Default) measure {seq(finca, programariego)}
    val parTime = withWarmer(new Default) measure {par(finca, programariego)}
       List(seqTime.value, parTime.value, seqTime.value/parTime.value)
    }

  def compararCostoMovilidad(seq: (Finca, ProgRiego, Distancia)=> Int, par: (Finca, ProgRiego, Distancia)=> Int)(finca: Finca, programariego: ProgRiego, distancias: Distancia): List[Double] = {
    val seqTime = withWarmer(new Default) measure {seq(finca, programariego, distancias)}
    val parTime = withWarmer(new Default) measure {par(finca, programariego, distancias)}
       List(seqTime.value, parTime.value, seqTime.value/parTime.value)
    }
  
  def compararGeneracionesDeRiego(seq: (Finca) => Vector[ProgRiego], par: (Finca) => Vector[ProgRiego]) (finca: Finca): List[Double] = {
    val seqTime = withWarmer(new Default) measure {seq(finca)}
    val parTime = withWarmer(new Default) measure {par(finca)}
    List(seqTime.value, parTime.value, seqTime.value/parTime.value)
  }

  def compararProgramacionRiegoOptimo(seq: (Finca, Distancia)=> (ProgRiego, Int), par: (Finca, Distancia)=> (ProgRiego, Int)) (finca: Finca, distancia: Distancia): List[Double] = {
    val seqTime = withWarmer(new Default) measure {seq(finca, distancia)}
    val parTime = withWarmer(new Default) measure {par(finca, distancia)}
    List(seqTime.value, parTime.value, seqTime.value/parTime.value)
  }

  
}
