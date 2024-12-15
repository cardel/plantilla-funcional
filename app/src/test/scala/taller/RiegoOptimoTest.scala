package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

@RunWith(classOf[JUnitRunner])
class RiegoOptimoTest extends AnyFunSuite {
  val objRiego = new RiegoOptimo()

  test("Generar Programaciones de Riego 5") {
    val f = Vector((10,3,4), (5,4,4), (2,2,1), (8,8,1), (6,4,2))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }

  test("Generar Programaciones de Riego 1") {
    val f = Vector((10,3,4), (5,4,4), (8,8,1), (6,4,2))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }

  test("Generar Programaciones de Riego 2") {
    val f = Vector((7,3,4), (6,4,4), (3,2,1), (8,2,1), (6,4,2), (8,2,4))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }

  test("Generar Programaciones de Riego 3") {
    val f = Vector((10,3,4), (8,8,1), (6,4,2))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }

  test("Generar Programaciones de Riego 4") {
    val f = Vector((10,3,4), (1,1,2), (3,8,9), (2,7,1), (2,8,3), (1,2,3), (3,2,1))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }


  // test("Generar Programaciones de Riego Par 1") {
  //   val f = Vector((10,3,4), (5,4,4), (2,2,1), (8,8,1), (6,4,2))
  //   val prog = objRiego.generarProgramacionesRiego(f)
  //   assert(prog == (0 until f.length).toVector.par.permutations.toVector, "si")
  // }
  //
  // test("Generar Programaciones de Riego Par 2") {
  //   val f = Vector((10,3,4), (5,4,4), (8,8,1), (6,4,2))
  //   val prog = objRiego.generarProgramacionesRiego(f)
  //   assert(prog == (0 until f.length).toVector.par.permutations.toVector, "si")
  // }
  //
  // test("Generar Programaciones de Riego Par 3") {
  //   val f = Vector((7,3,4), (6,4,4), (3,2,1), (8,2,1), (6,4,2), (8,2,4))
  //   val prog = objRiego.generarProgramacionesRiego(f)
  //   assert(prog == (0 until f.length).toVector.par.permutations.toVector, "si")
  // }
  //
  // test("Generar Programaciones de Riego Par 4") {
  //   val f = Vector((10,3,4), (8,8,1), (6,4,2))
  //   val prog = objRiego.generarProgramacionesRiego(f)
  //   assert(prog == (0 until f.length).toVector.par.permutations.toVector, "si")
  // }
  //
  // test("Generar Programaciones de Riego Par 5") {
  //   val f = Vector((10,3,4), (1,1,2), (3,8,9), (2,7,1), (2,8,3), (1,2,3), (3,2,1))
  //   val prog = objRiego.generarProgramacionesRiego(f)
  //   assert(prog == (0 until f.length).toParVector.par.permutations.toVector, "si")
  // }
  
  //tests para ProgramacionRiegoOptima
  test("test_1 ProgramaciónRiegoOptima") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3))
    val distancias = Vector(Vector(0, 1, 2), Vector(1, 0, 1), Vector(2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptima(finca, distancias)
    assert(res == Vector(0, 1, 2), "Programación de riego óptima")
  }

  test("test_2 ProgramaciónRiegoOptima") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4))
    val distancias = Vector(Vector(0, 1, 2, 3), Vector(1, 0, 1, 2), Vector(2, 1, 0, 1), Vector(3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptima(finca, distancias)
    assert(res == Vector(0, 1, 2, 3), "Programación de riego óptima")
  }

  test("test_3 ProgramaciónRiegoOptima") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5))
    val distancias = Vector(Vector(0, 1, 2, 3, 4), Vector(1, 0, 1, 2, 3), Vector(2, 1, 0, 1, 2), Vector(3, 2, 1, 0, 1), Vector(4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptima(finca, distancias)
    assert(res == Vector(0, 1, 2, 3, 4), "Programación de riego óptima")
  }

  test("test_4 ProgramaciónRiegoOptima") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5), (6, 6, 6))
    val distancias = Vector(Vector(0, 1, 2, 3, 4, 5), Vector(1, 0, 1, 2, 3, 4), Vector(2, 1, 0, 1, 2, 3), Vector(3, 2, 1, 0, 1, 2), Vector(4, 3, 2, 1, 0, 1), Vector(5, 4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptima(finca, distancias)
    assert(res == Vector(0, 1, 2, 3, 4, 5), "Programación de riego óptima")
  }

  test("test_5 ProgramaciónRiegoOptima") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5), (6, 6, 6), (7, 7, 7))
    val distancias = Vector(Vector(0, 1, 2, 3, 4, 5, 6), Vector(1, 0, 1, 2, 3, 4, 5), Vector(2, 1, 0, 1, 2, 3, 4), Vector(3, 2, 1, 0, 1, 2, 3), Vector(4, 3, 2, 1, 0, 1, 2), Vector(5, 4, 3, 2, 1, 0, 1), Vector(6, 5, 4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptima(finca, distancias)
    assert(res == Vector(0, 1, 2, 3, 4, 5, 6), "Programación de riego óptima")
  }

//test para ProgramacionRiegoOptima Paralelizada
  test("test_1 ProgramacionRiegoOptimaParalelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3))
    val distancias = Vector(Vector(0, 1, 2), Vector(1, 0, 1), Vector(2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == Vector(0, 1, 2), "Programación de riego óptima")
  }

  test("test_2 ProgramacionRiegoOptimaParalelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4))
    val distancias = Vector(Vector(0, 1, 2, 3), Vector(1, 0, 1, 2), Vector(2, 1, 0, 1), Vector(3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == Vector(0, 1, 2, 3), "Programación de riego óptima")
  }

  test("test_3 ProgramacionRiegoOptimaParalelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5))
    val distancias = Vector(Vector(0, 1, 2, 3, 4), Vector(1, 0, 1, 2, 3), Vector(2, 1, 0, 1, 2), Vector(3, 2, 1, 0, 1), Vector(4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == Vector(0, 1, 2, 3, 4), "Programación de riego óptima")
  }

  test("test_4 ProgramacionRiegoOptimaParalelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5), (6, 6, 6))
    val distancias = Vector(Vector(0, 1, 2, 3, 4, 5), Vector(1, 0, 1, 2, 3, 4), Vector(2, 1, 0, 1, 2, 3), Vector(3, 2, 1, 0, 1, 2), Vector(4, 3, 2, 1, 0, 1), Vector(5, 4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == Vector(0, 1, 2, 3, 4, 5), "Programación de riego óptima")
  }

  test("test_5 ProgramacionRiegoOptimaParalelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5), (6, 6, 6), (7, 7, 7))
    val distancias = Vector(Vector(0, 1, 2, 3, 4, 5, 6), Vector(1, 0, 1, 2, 3, 4, 5), Vector(2, 1, 0, 1, 2, 3, 4), Vector(3, 2, 1, 0, 1, 2, 3), Vector(4, 3, 2, 1, 0, 1, 2), Vector(5, 4, 3, 2, 1, 0, 1), Vector(6, 5, 4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == Vector(0, 1, 2, 3, 4, 5, 6), "Programación de riego óptima")
  }
}