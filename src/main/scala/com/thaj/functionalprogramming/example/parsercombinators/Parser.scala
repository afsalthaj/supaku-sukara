package com.thaj.functionalprogramming.example.parsercombinators

import argonaut.EncodeJson
import argonaut._, Argonaut._

import scalaz._, Scalaz._

import com.thaj.functionalprogramming.example.parsercombinators.Config._
/**
  * Created by afsalthaj on 14/10/2016.
  */
// A JSON PARSER. A JSON parser may consist of reading an input file
// This JSON consist of a feature name, feature type and feature value
// To have better type safety, featurename and freshness is a value class
// Feature Type can be String, Type or Double and we define our own types


// First component is to read a JSON file
// Second component is to operate on the content of JSON file, and validate the same

case class Config(map: Map[FeatureName, FeatureInfo])

object Config {
  type JsonFilePath = String
  type JsonString = String
  type FeatureName = String
  type Freshness = Int
  type Primitives = String
  case class FeatureInfo(valueType: FeatureType, freshness: Freshness)
}

sealed trait FeatureType

object FeatureType {
  case object String extends FeatureType
  case object Int extends FeatureType
  case object Double extends FeatureType
  case object Date extends FeatureType

  def decodeValueType(x: Primitives) = x match {
    case "int" => Int
    case "double" => Double
    case "date" => Date
    case "string" => String
  }
}

object JsonParser {
  import FeatureType._

  implicit def ConfigDecodeJson: DecodeJson[(FeatureName, FeatureInfo)] =
    DecodeJson(c => for {
      featureName <- (c --\ "featureName").as[String]
      freshness <- (c --\ "freshness").as[Int]
      valueType <- (c --\ "valueType").as[String]
    } yield (featureName, FeatureInfo(decodeValueType(valueType), freshness))
    )

  // Decode ignoring error messages
  def getConfigFromJson(rawJson: JsonString): Validation[String, Config] =
  Parse.decodeValidation[List[(FeatureName, FeatureInfo)]](rawJson).map(_.toMap).map(Config(_))
}
