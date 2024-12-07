package taller

class MultMatriz () {

    def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
        val n = m1.length
        
        if (n == 1) {
            Vector(Vector(m1(0)(0) * m2(0)(0)))
        } 
        
        else {
            val mitad = n/2
            
            
            val a11 = subMatriz(m1, 0, 0, mitad)
            val a12 = subMatriz(m1, 0, mitad, mitad)
            val a21 = subMatriz(m1, mitad, 0, mitad)
            val a22 = subMatriz(m1, mitad, mitad, mitad)
            
            val b11 = subMatriz(m2, 0, 0, mitad)
            val b12 = subMatriz(m2, 0, mitad, mitad)
            val b21 = subMatriz(m2, mitad, 0, mitad)
            val b22 = subMatriz(m2, mitad, mitad, mitad)
            
            val p1 = SumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
            val p2 = SumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
            val p3 = SumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
            val p4 = SumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

            Vector.tabulate(n, n)((i, j) => 
                if (i < mitad && j < mitad) p1(i)(j)
                else if (i < mitad && j >= mitad) p2(i)(j - mitad)
                else if (i >= mitad && j < mitad) p3(i - mitad)(j)
                else p4(i - mitad)(j - mitad)
            )
        }
    }
}