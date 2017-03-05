package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 2,
    Key.exec.maxWarmupRuns -> 3,
    Key.exec.benchRuns -> 4,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 1000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    var res = 0F
    for(i<-input.indices if i != 0){
      val t = max(res,input(i)/i)
      res = t
      output(i) = t
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    def tail(input: Array[Float], from: Int, until: Int,res: Float): Float ={
      if(until == from || input.length < from+1)
        res
      else tail(input,from+1,until,max(input(from)/from,res))

    }
    tail(input,from,until,0F)
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if(end-from<=threshold)
      Leaf(from,end,upsweepSequential(input,from,end))
    else{
      val mid = from + ((end - from) /2)
      val (lt,rt) = parallel(upsweep(input,from,mid,threshold),upsweep(input,mid,end,threshold))
      Node(lt,rt)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    if(until == from || input.length < from+1)
       return
    else if(from==0) downsweepSequential(input,output,output(from),from+1,until)
    else{
      output(from) = max(input(from)/from,startingAngle)
      downsweepSequential(input,output,output(from),from+1,until)
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit
  = tree match {
    case Leaf(f, u, m) => downsweepSequential(input,output,m,f,u)
    case Node(l,r) => parallel(downsweep(input,output,startingAngle,l),downsweep(input,output,l.maxPrevious,r))
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val u = upsweep(input,0,input.length,threshold)
    downsweep(input,output,0f,u)
  }
}
