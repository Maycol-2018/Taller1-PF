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


  def splitMultiply(a: Int, b: Int): Int = {
    // Caso base: si a o b tienen un solo dígito, multiplicamos directamente
    if (a < 10 || b < 10) {
      a * b
    } else {
      // Función para contar dígitos
      def countDigits(num: Int, count: Int = 0): Int = {
        if (num == 0) count else countDigits(num / 10, count + 1)
      }

      // Calculamos el número de dígitos de a y b
      val digitsA = countDigits(a)
      val digitsB = countDigits(b)
      val n = if (digitsA > digitsB) digitsA else digitsB
      val m = n / 2

      // Calculamos 10^m usando math.pow
      val pow10m = math.pow(10, m).toInt

      // Descomponemos a en x e y
      val x = a / pow10m
      val y = a % pow10m

      // Descomponemos b en z y w
      val z = b / pow10m
      val w = b % pow10m

      // Multiplicaciones recursivas
      val xz = splitMultiply(x, z)
      val yw = splitMultiply(y, w)
      val xw = splitMultiply(x, w)
      val yz = splitMultiply(y, z)

      // Calculamos 10^(2m) usando math.pow
      val tenPow2m = math.pow(10, 2 * m).toInt

      // Aplicamos la fórmula del SplitAlgorithm
      tenPow2m * xz + pow10m * (xw + yz) + yw
    }
  }

  // Devuelve la multiplicación de dos enteros recursivos usando el FastAlgorithm (Karatsuba)
  def fastMultiply(a: Int, b: Int): Int = {
    // Condición base: si alguno de los números es de un solo digito, usar multiplicación directa
    if (a < 10 || b < 10) a * b
    else {
      // Función para contar los dígitos del número
      def numLength(n: Int): Int = if (n < 10) 1 else 1 + numLength(n / 10)

      // Función para dividir un número en dos partes
      def decomposeNumber(num: Int, tenPow: Int): (Int, Int) = (num / tenPow, num % tenPow)

      // Función para combinar los resultados del algoritmo de Karatsuba
      def joinResults(z0: Int, z1: Int, z2: Int, tenPow: Int): Int = {
        z2 * (tenPow * tenPow) + (z1 - z2 - z0) * tenPow + z0
      }

      // Calcular el número de dígitos del número más grande
      val numDigitsA = numLength(a)
      val numDigitsB = numLength(b)
      val m = if (numDigitsA > numDigitsB) numDigitsA else numDigitsB
      val m2 = m / 2

      // Calcular 10^m2 usando math.pow
      val tenPow = math.pow(10, m2).toInt

      // Dividir los números en dos partes
      val (part1A, part2A) = decomposeNumber(a, tenPow)
      val (part1B, part2B) = decomposeNumber(b, tenPow)

      // Aplicar recursivamente el algoritmo de Karatsuba
      val z0 = fastMultiply(part2A, part2B)
      val z1 = fastMultiply((part1A + part2A), (part1B + part2B))
      val z2 = fastMultiply(part1A, part1B)

      // Combinar los resultados
      joinResults(z0, z1, z2, tenPow)
    }
  }

}
