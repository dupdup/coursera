package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }
  test("parlineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output,1)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }
  test("parlineOfSight should correctly handle an array of size 45") {
    val output = new Array[Float](5)
//    List(0.0, 12.0, 12.0, 12.0, 12.0) did not equal List(0.0, 7.0, 7.0, 11.0, 12.0)
    parLineOfSight(Array[Float](0f, 7f, 5f, 33f, 48f), output,2)
    assert(output.toList == List(0.0, 7.0, 7.0, 11.0, 12.0))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is 5f") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 5f, 1, 4)
    assert(output.toList == List(0f, 5f, 5f, 5f))
  }

}

