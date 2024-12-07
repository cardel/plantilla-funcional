package taller


class SumMatriz() {
    def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
        // recibe m1 y m2 matrices cuadradas de la misma dimension , potencia,→ de 2
        val n = m1.length
        require(m1.length == m2.length && m1(0).length == m2(0).length)
        val resultado = Vector.tabulate(n, n)((i, j) => m1(i)(j) + m2(i)(j))
        // y devuelve la matriz resultante de la suma de las 2 matrices
        resultado
    }
}

