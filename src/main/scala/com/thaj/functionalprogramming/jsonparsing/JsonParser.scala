package com.thaj.functionalprogramming.example.jsonparsing

import argonaut._, Argonaut._

import scalaz._, Scalaz._

import com.thaj.functionalprogramming.example.jsonparsing.Config._
/**
 * Created by afsalthaj on 14/10/2016.
 */

// The project is to show an example of a real life scala code
// Makes use of scalaz validations (you can see it as Validation defined in this project)
// Project also make use of scalaz \/, which you can see it as Either for the time being
case class Config(map: Map[FeatureName, FeatureInfo])

object Config {
  type FeatureName = String
  type Freshness = Int
  case class FeatureInfo(valueType: FeatureType, freshness: Freshness)

}

sealed trait FeatureType

object FeatureType {
  case object String extends FeatureType
  case object Int extends FeatureType
  case object Double extends FeatureType
  case object Date extends FeatureType

  def fromString(x: String): String \/ FeatureType = x match {
    case "Int"    => Int.right[String]
    case "Double" => Double.right[String]
    case "Date"   => Date.right[String]
    case "String" => String.right[String]
    case _        => s"unknown type $x".left[FeatureType]
  }
}

object JsonParser {
  import FeatureType._

  implicit def FeatureTypeDecodeJson: DecodeJson[FeatureType] =
    DecodeJson(c =>
      for {
        valTypeStr <- c.as[String]
        featureType <- fromString(valTypeStr).fold(t => DecodeResult.fail(t, c.history), DecodeResult.ok)
      } yield featureType)

  implicit def ConfigDecodeJson: DecodeJson[(FeatureName, FeatureInfo)] =
    DecodeJson(
      c => for {
        featureName <- (c --\ "featureName").as[FeatureName]
        freshness <- (c --\ "freshness").as[Freshness]
        valueType <- (c --\ "valueType").as[FeatureType]
      } yield (featureName, FeatureInfo(valueType, freshness)))

  def getConfigFromJson(jsonString: String): Validation[String, Config] = for {
    list <- Parse.decodeValidation[List[(FeatureName, FeatureInfo)]](jsonString)
  } yield Config(list.toMap)
}