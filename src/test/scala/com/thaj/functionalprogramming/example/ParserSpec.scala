package com.thaj.functionalprogramming.example

import org.specs2.Specification

import com.thaj.functionalprogramming.example.jsonparsing.FeatureType
import com.thaj.functionalprogramming.example.jsonparsing.Config
import com.thaj.functionalprogramming.example.jsonparsing.JsonParser._
import com.thaj.functionalprogramming.example.jsonparsing.Config.FeatureInfo

import scalaz._, Scalaz._

class ParserSpec extends Specification {
  def is =
    s"""
      |test readParseandMapToConfig $testFeatureParser
      |test readParseandMapToConfig negaiveTestFeatureParser
    """.stripMargin

  def testFeatureParser = {
    val rawJson =
      """
        | [{
        | 	"featureName": "ah..some..feature",
        | 	"freshness": 2,
        | 	"valueType": "Int"
        | }, {
        | 	"featureName": "some other",
        | 	"freshness": 3,
        | 	"valueType": "String"
        | }]
      """.stripMargin

    assert(getConfigFromJson(rawJson) == Success(Config(
      Map(
        "ah..some..feature" -> FeatureInfo(FeatureType.Int, 2),
        "some other" -> FeatureInfo(FeatureType.String, 3)
      )
    )))
  }

  def negaiveTestFeatureParser = {
    val rawJson =
      """
        | [{
        | 	"featureName": "ah..some..feature",
        | 	"freshness": 2,
        | 	"valueType": "Int"
        | }, {
        | 	"featureName": "some other",
        | 	"freshness": "4",
        | 	"valueType": "String"
        | }]
      """.stripMargin

    getConfigFromJson(rawJson) must_== Success(Config(
      Map(
        "ah..some..feature" -> FeatureInfo(FeatureType.Int, 2),
        "some other" -> FeatureInfo(FeatureType.String, 4)
      )
    ))
  }
}