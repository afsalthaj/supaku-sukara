package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.example.exercises.{ Exercise }
import org.scalatest.WordSpec
import org.scalatest.prop.{ Checkers, PropertyChecks }
import org.scalatest.MustMatchers._
import org.specs2.Specification

class OptionProp extends WordSpec with PropertyChecks with Checkers {
  import com.thaj.functionalprogramming.example.exercises.OptionOperation._
  "Option filter" must {
    "have passed" in {
      forAll { (w: String) =>
        whenever(w != "afsa") {
          Some("afsa").filter(_ == w) == None
        }
      }
    }
  }
}

object OptionSpec extends Specification {
  import com.thaj.functionalprogramming.example.exercises.OptionOperation, OptionOperation._
  def is =
    s"""
      |${Option.sequence(List(Some(1), Some(2))) == Some(List(1, 2))}
      |${Option.sequence(List(Some(1), None)) == None}
      |${Exercise.traverse(List(1))(t => Some(1)) == Some(List(1))}
      |${Exercise.traverse(List(1))(t => None) == None}
      |${Exercise.traverse(List(1, 2))(t => if (t == 1) Some(t) else None) == None}
      |${Exercise.traverse(List(1, 2))(t => Some(t)) == Some(List(1, 2))}
      |${val f = Exercise.lift((c: Int) => c + 1); f(Some(1)) == Some(2)}
      |${Exercise.map2(Some(1), Some(2))((a, b) => a + b) == Some(3)}
    """.stripMargin
}

object ValidationSpec extends Specification {
  import com.thaj.functionalprogramming.example.exercises._, EitherOperations._

  def is =
    s"""
      |${
      assert(convertListOfValidationToValidation ==
        Error(List(
          "For input string: \"b\"",
          "For input string: \"a\""
        )))
    }
      |${
      assert(converListOFEitherToEither must_==
        Left("For input string: \"a\""))
    }
    """.stripMargin

  def convertStringToInt: List[Validation[String, Int]] = {
    val list = List("1", "2", "3", "a", "b")
    list.map(t => { Validation.Try(t.toInt) })
  }

  def convertListOfValidationToValidation: Validation[String, List[Int]] =
    Validation.sequence(convertStringToInt)

  def convertStringToIntThroughEither: List[Either[String, Int]] = {
    val list = List("1", "2", "3", "a", "b")
    list.map(t => Either.Try(t.toInt))
  }

  def converListOFEitherToEither: Either[String, List[Int]] =
    Either.sequence(convertStringToIntThroughEither)
}