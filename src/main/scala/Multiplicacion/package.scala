package object Multiplicacion {
  
  def PeasantAlgorithm(x: Int, y: Int): Int = {
    // Version recursiva lineal del Peasant Algorithm para multiplicar dos enteros positivos
    if (x == 0) {
      x
    }
    else if (x % 2 == 0) {
      PeasantAlgorithm(x / 2, y + y)
    }
    else {
      y + PeasantAlgorithm(x / 2, y + y)
    }
  }

  def PeasantAlgorithmV2(x: Int, y: Int, acc: Int = 0): Int = {
    // Version iterativa lineal del Peasant Algorithm para multiplicar dos enteros positivos
    if (x == 0) acc
    else if (x % 2 == 0) PeasantAlgorithmV2(x / 2, y * 2, acc) // Si x es par, dividir y multiplicar
    else PeasantAlgorithmV2(x / 2, y * 2, acc + y) // Si x es impar, acumular y
  }
  
  

}
