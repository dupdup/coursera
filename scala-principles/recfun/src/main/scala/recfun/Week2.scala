package recfun

/**
  * Created by doruk on 11/14/16.
  */
object Week2 {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a>b) acc
      else loop(a+1, acc+f(a))
    }
    loop(a, 0)
  }
  def main(args: Array[String]) {
    print (sum(x => x)(1, 3))
  }
}
