def PeasantAlgorithm(x:Int, y:Int): Int = {
  if(x == 0){
    x
  }
  else if (x%2 == 0){
    PeasantAlgorithm(x/2,y+y)
  }
  else{
    PeasantAlgorithm(x/2,y+y) + y
  }
}

def PeasantAlgorithmIt(x:Int, y:Int):Int={
  var result = 0
  var a = x
  var b = y
  for(_ <- 0 to 5){
    a /= 2
    b += b
    if(a%2!=0){
      result += b;
    }
  }
  result
}


PeasantAlgorithmIt(2,4)
