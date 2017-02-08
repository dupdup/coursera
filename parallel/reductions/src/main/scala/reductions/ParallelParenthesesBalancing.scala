package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000
    val chars = new Array[Char](length)
    val threshold = 10
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def tb(c: Int, cs: Array[Char]): Boolean = {
      if (cs.isEmpty) c == 0
      else if (cs.head == '(') tb(c + 1, cs.tail)
      else if (cs.head == ')' && c > 0) tb(c - 1, cs.tail)
      else if (cs.head == ')' && c == 0) false
      else tb(c, cs.tail)
    }
    tb(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, oc: Int, cc: Int): Int= {
      if(idx == until) oc - cc
      else if(chars(idx) == '(') traverse(idx+1,until,oc+1,cc)
      else if(chars(idx) == ')') traverse(idx+1,until,oc,cc+1)
      else traverse(idx+1,until,oc,cc)
    }

    def reduce(from: Int, until: Int): Int= {
      if(until-from <= threshold)
        traverse(from,until,0,0)
      else{
        val mid = (from + until) / 2
        val r = parallel(reduce(from,mid),reduce(mid,until))
        if(r._1 < 0) r._1
        else r._1 + r._2
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
