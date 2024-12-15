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
}
