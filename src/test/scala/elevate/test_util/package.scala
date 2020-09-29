package elevate

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

package object test_util {
  abstract class Tests extends AnyFunSuite with Matchers
}
