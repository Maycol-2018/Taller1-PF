package object Multiplicacion {
  
  def PeasantAlgorithm(x: Int, y: Int): Int = {
    if (x == 0) {
      x
    }
    else if (x % 2 == 0) {
      PeasantAlgorithm(x / 2, y + y)
    }
    else {
      PeasantAlgorithm(x / 2, y + y) + y
    }
  }

  def PeasantAlgorithmIt(x: Int, y: Int): Int = {
    var result = 0
    var a = x
    var b = y
    for (_ <- 0 to 5) {
      a /= 2
      b += b
      if (a % 2 != 0) {
        result += b;
      }
    }
    result
  }


  PeasantAlgorithmIt(2, 4)

  // Devuelve la multiplicación de dos enteros recursivos usando el FastAlgorithm (Karatsuba)
  def fastMultiply(a: Int, b: Int): Int = {
    // Condición base: si alguno de los números es pequeño, usar multiplicación directa
    if (a < 10 || b < 10) a * b
    else {
      // Función para contar los dígitos de un número
      def countDigits(n: Int): Int = if (n < 10) 1 else 1 + countDigits(n / 10)

      // Función para dividir un número en dos partes
      def splitNumber(num: Int, powerOf10: Int): (Int, Int) = (num / powerOf10, num % powerOf10)

      // Función para combinar los resultados del algoritmo de Karatsuba
      def combineResults(z0: Int, z1: Int, z2: Int, powerOf10: Int): Int = {
        z2 * (powerOf10 * powerOf10) + (z1 - z2 - z0) * powerOf10 + z0
      }

      // Calcular el número de dígitos del número más grande
      val numDigitsA = countDigits(a)
      val numDigitsB = countDigits(b)
      val m = if (numDigitsA > numDigitsB) numDigitsA else numDigitsB
      val m2 = m / 2

      // Calcular 10^m2 usando math.pow
      val powerOf10 = math.pow(10, m2).toInt

      // Dividir los números en dos partes
      val (part1A, part2A) = splitNumber(a, powerOf10)
      val (part1B, part2B) = splitNumber(b, powerOf10)

      // Aplicar recursivamente el algoritmo de Karatsuba
      val z0 = fastMultiply(part2A, part2B)
      val z1 = fastMultiply((part1A + part2A), (part1B + part2B))
      val z2 = fastMultiply(part1A, part1B)

      // Combinar los resultados
      combineResults(z0, z1, z2, powerOf10)
    }
  }

}
