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
  // assert(1 + 2 === 3)
  //}


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
    * val s1 = singletonSet(1)
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

  test("union - contains all elements from UNION of sets: s1 and s2") {
    new TestSets {
      //calculate union
      val unions1s2 = union(s1, s2) //unions1s2 contains all elements from UNION of sets: s1 and s2
      assert(contains(unions1s2, 1), "Union contains 1")
      assert(contains(unions1s2, 2), "Union contains 2")
      assert(!contains(unions1s2, 3), "Union contains 3")
      assert(!contains(unions1s2, 4), "Union not contains 4, expected false")

      val unions2s3 = union(s2, s3) //unions2s3 contains all elements from UNION of sets: s2 and s3
      assert(!contains(unions2s3, 1), "Union not contains 1")
      assert(contains(unions2s3, 2), "Union contains 2")
      assert(contains(unions2s3, 3), "Union contains 3")
      assert(!contains(unions1s2, 4), "Union not contains 4, expected false")
    }
  }

  test("intersect - contains only Intersection elements between union sets: unions1s2 and unions2s3") {
    new TestSets {
      //calculate union
      val unions1s2 = union(s1, s2)
      //unions1s2 contains all elements from UNION of sets: s1 and s2
      val unions2s3 = union(s2, s3) //unions2s3 contains all elements from UNION of sets: s2 and s3

      val intersatcions1s2s3 = intersect(unions1s2, unions2s3) //intersatcions1s2s3 contains only Intersection elements between union sets: us1s2 and us2s3
      assert(!contains(intersatcions1s2s3, 1), "Intersect not contains 1")
      assert(contains(intersatcions1s2s3, 2), "Intersect contains 2")
      assert(!contains(intersatcions1s2s3, 3), "Intersect not contains 3")
      assert(!contains(intersatcions1s2s3, 4), "Intersect not contains 4, expected false")
    }
  }

  test("difference - contains a set with elements in one set but not in the other set") {
    new TestSets {
      //calculate union for all sets
      val unions1s2 = union(s1, s2)
      //unions1s2 contains all elements from UNION of sets: s1 and s2
      val unions1s2s3 = union(unions1s2, s3) //unions1s2s3 contains all elements from all sets

      val differences1s2s3 = diff(unions1s2s3, s1) //contains a set with elem in unions1s2s3 but not in s1
      assert(!contains(differences1s2s3, 1), "difference contains 3 2 ")
      assert(contains(differences1s2s3, 2), "difference contains 1 3")
      assert(contains(differences1s2s3, 3), "difference contains 1 2")
      assert(!contains(differences1s2s3, 4), "Intersect not contains 1 2 3, expected false")
    }
  }

  test("filter - filter which takes a predicate function as param and return a subset witch matches the predicate") {
    new TestSets {
      //calculate union for all sets
      val unions1s2 = union(s1, s2)
      //unions1s2 contains all elements from UNION of sets: s1 and s2
      val unions1s2s3 = union(unions1s2, s3) //unions1s2s3 contains all elements from all sets

      val filters1s2s3 = filter(unions1s2s3, (i1: Int) => i1 % 2 == 0)
      assert(!contains(filters1s2s3, 1), "Filter by 1")
      assert(contains(filters1s2s3, 2),  "Filter by 2") //i1 % 2 == 0
      assert(!contains(filters1s2s3, 3), "Filter by 3")
      assert(!contains(filters1s2s3, 4), "Filter by 4") //not in set
    }
  }

  test("map") {
    new TestSets {
      val testMap = map(s1, str => 2 * str )
      assert(contains(testMap, 2), "Map by 2")
      assert(!contains(testMap, 3), "Map by 3")
    }
  }
}
