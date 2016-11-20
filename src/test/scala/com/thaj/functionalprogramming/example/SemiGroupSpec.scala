package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.example.typeclass.SemigroupSimple.ops._
import org.specs2.Specification
object SemiGroupSpec extends Specification {

def is =
  s"""
    |${("afsal" |+| "hi") == "afsalhi"}
    |${(1 |+| 2) == 12}
  """.stripMargin
}