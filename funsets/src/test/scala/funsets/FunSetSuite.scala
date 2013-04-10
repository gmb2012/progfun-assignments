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
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
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

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect test") {
    new TestSets {
      val s = intersect(s1, s1)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff test") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter test") {
    new TestSets {
      val s = filter(s1, _ < 2)
      assert(contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
    }
  }

  test("forall test") {
    new TestSets {
      assert(forall(s1, _ < 2), "Forall 1")
      assert(!forall(s2, _ < 2), "Forall 2")
      assert(!forall(s3, _ < 2), "Forall 3")
    }
  }

  test("exists test") {
    new TestSets {
      assert(exists(s1, _ < 3), "Exists 1")
      assert(exists(s2, _ < 3), "Exists 2")
      assert(!exists(s3, _ < 3), "Exists 3")
    }
  }


  test("diff of {1,3,4,5,7,1000} and {1,2,3,4}") {
    val a1 = union(Set(1), Set(3))
    val a2 = union(Set(4), Set(5))
    val a3 = union(Set(7), Set(1000))
    val a4 = union(a1, a2)
    val src = union(a3, a4)
    val b1 = union(Set(1), Set(2))
    val b2 = union(Set(3), Set(4))
    val dest = union(b1, b2)
    // printSet(src)
    // printSet(dest)
    val result = diff(src, dest)
    // printSet(result)
    assert(result(5))
    assert(result(7))
    assert(result(1000))
  }

  test("diff of {1,2,3,4} and {-1000,0}") {
    val a1 = union(Set(1), Set(2))
    val a2 = union(Set(3), Set(4))
    val src = union(a1, a2)
    val dest = union(Set(-1000), Set(0))
    val result = diff(src, dest)
    assert(result(1))
    assert(result(2))
    assert(result(3))
    assert(result(4))
  }

  test("forall: {1,3,4,5,7,1000} _ < 5") {
    val a1 = union(Set(1), Set(3))
    val a2 = union(Set(4), Set(5))
    val a3 = union(Set(7), Set(1000))
    val a4 = union(a1, a2)
    val src = union(a3, a4)
    assert(!forall(src, _ < 5))
  }

  test("exists: given {1,3,4,5,7,1000} 2 shouldn't exist") {
    val a1 = union(Set(1), Set(3))
    val a2 = union(Set(4), Set(5))
    val a3 = union(Set(7), Set(1000))
    val a4 = union(a1, a2)
    val src = union(a3, a4)
    assert(!exists(src, _ == 2))
  }

  test("map: {1,3,4,5,7,1000}, _ + 1") {
    val a1 = union(Set(1), Set(3))
    val a2 = union(Set(4), Set(5))
    val a3 = union(Set(7), Set(1000))
    val a4 = union(a1, a2)
    val src = union(a3, a4)
    val result = map(src, _ + 1)
    // printSet(src)
    printSet(result)
  }

  test("forall & map: doubling numbers") {
    val a1 = union(Set(1), Set(3))
    val a2 = union(Set(4), Set(5))
    val a3 = union(Set(7), Set(1000))
    val a4 = union(a1, a2)
    val src = union(a3, a4)
    printSet(src)
    printSet(map(src, _ * 2))
  }
}
