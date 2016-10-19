package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.example.jsonparsing.Config.FeatureInfo
import org.specs2.Specification

import scalaz._, Scalaz._

class ParserSpec extends Specification {
  import com.thaj.functionalprogramming.example.jsonparsing.FeatureType
  import com.thaj.functionalprogramming.example.jsonparsing.Config
  import com.thaj.functionalprogramming.example.jsonparsing.JsonParser._

  def is =
    s"""
      |test readParseandMapToConfig $testArganautParser
    """.stripMargin

  def testArganautParser = {

    val rawJson =
      """
        |      [{
        |      	"featureName": "ah..some..feature",
        |      	"freshness": 2,
        |      	"valueType": "int"
        |      }, {
        |      	"featureName": "hi",
        |      	"freshness": 3,
        |      	"valueType": "string"
        |      }]
      """.stripMargin


    val config = getConfigFromJson(rawJson) match {
      case Success(a) => a
      case Failure(_) => throw new Exception("validation failed")
    }

    config == Config(
      Map(
        "ah..some..feature" -> FeatureInfo(FeatureType.Int,2),
        "hi" -> FeatureInfo(FeatureType.String,3)
      )
    )
  }
}
