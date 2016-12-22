package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
    assert(contains(singletonSet(1), 1))
    assert(!contains(singletonSet(2), 1))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val union12: Set = union(s1, s2)
    val union23: Set = union(s2, s3)
    val union123: Set = union(union(s1, s2),s3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val us = union(s1, s2)
      assert(contains(us, 1), "Union 1")
      assert(contains(us, 2), "Union 2")
      assert(!contains(us, 3), "Union 3")
    }
  }
  test("intersect contains all elements that are both in `s` and `t`") {
    new TestSets {

      val is = intersect(union12,union23)
      assert(!contains(is, 1), "Union 1")
      assert(contains(is, 2), "Union 2")
      assert(!contains(is, 3), "Union 3")
    }
  }
  test("diff contains all elements of `s` that are not in `t`") {
    new TestSets {
      val is = diff(union12,union23)
      assert(contains(is, 1), "Union 1")
      assert(!contains(is, 2), "Union 2")
      assert(!contains(is, 3), "Union 3")
    }
  }
  test("filter returns the subset of `s` for which `p` holds") {
    new TestSets {
      val is = filter(union123,(x:Int)=>x>1)
      assert(!contains(is, 1), "Union 1")
      assert(contains(is, 2), "Union 2")
      assert(contains(is, 3), "Union 3")
      assert(!contains(is, 4), "Union 3")
    }
  }
  test("forall returns whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      assert(forall(union123,(x:Int)=>x>0), "Union 1")
      assert(!forall(union123,(x:Int)=>x>1), "Union 2")
      assert(forall(union123,(x:Int)=>x<4), "Union 3")
    }
  }
  test("exists returns whether there exists a bounded integer within `s` that satisfies `p`") {
    new TestSets {
      assert(exists(union123,(x:Int)=>x>0), "Union 1")
      assert(exists(union123,(x:Int)=>x>1), "Union 2")
      assert(!exists(union123,(x:Int)=>x>4), "Union 3")
    }
  }
  test("map returns a set transformed by applying `f` to each element of `s`.") {
    new TestSets {
      assert(forall(map(union12,(x:Int)=>x+1) ,(a:Int)=>a>1), "Union 1")
      assert(exists(map(s1,(x:Int)=>x+2),_==3), "Union 2")
    }
  }

}
