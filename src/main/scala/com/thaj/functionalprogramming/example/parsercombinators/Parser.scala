package com.thaj.functionalprogramming.example.parsercombinators

/**
  * Created by afsalthaj on 14/10/2016.
  */
// A JSON PARSER. A JSON parser may consist of reading an input file
// This JSON consist of a feature name, feature type and feature value
// To have better type safety, featurename and freshness is a value class
// Feature Type can be String, Type or Double and we define our own types


// First component is to read a JSON file
// Second component is to operate on the content of JSON file, and validate the same

object Parser {
  sealed trait FeatureType[T]
  object FeatureType {
    object String extends FeatureType[String]
    object Int extends FeatureType[Int]
    object Double extends FeatureType[Double]
  }

  case class FeatureName(featureName: String) extends AnyVal
  case class Freshness(freshNess: Int) extends AnyVal
  case class JsonFilePath(path: String) extends AnyVal


  val sampleJsonString =
  def readJson(readJson: JsonFilePath)
}
