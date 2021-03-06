object exercise {
  def product(f: Int => Int)(a: Int, b: Int):Int = {
    if (a > b ) 1
    else f(a) * product(f)(a + 1, b)
  }
  // println(product(x => x * x)(3, 4))

  def factorial(n: Int):Int = product(x => x)(1, n)
  // println(factorial(5)) // 120

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a:Int, b:Int): Int = {
    if( a > b ) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }
  println(mapReduce(x => x * x, _+_, 0)(3, 4))
}
