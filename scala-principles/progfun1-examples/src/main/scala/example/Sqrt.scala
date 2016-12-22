package example

/**
  * Created by doruk on 11/9/16.
  */
object Sqrt {
  def abs(x:Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) = abs(x / guess - guess) < 0.1

  def improve(guess: Double, x: Double) =(guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

  def main(args: Array[String]) {
    println(sqrt(1e50))
  }

}
