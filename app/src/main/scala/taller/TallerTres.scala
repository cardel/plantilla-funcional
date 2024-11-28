package taller

class TallerTres {
  type Matriz = Vector[Vector[Int]]
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    // Extrae las filas relevantes de la matriz m
    m.slice(i, i + l).map(row => row.slice(j, j + l))
  }
}