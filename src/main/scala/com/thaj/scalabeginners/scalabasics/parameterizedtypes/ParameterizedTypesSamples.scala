package com.scalabasics.parameterizedtypes

/**
 * in java community it is generics and in scala community it is parameterized types.
 */
object ParameterizedTypesSamples {

  def main(args: Array[String]) {
    //in java community it is generics and in scala community it is parameterized types. 

    val firstNameLastName: List[String] = List("Afsal", "thaj")
    val maprFirstNameLastName: Map[String, String] = Map("Afsal" -> "thaj")

    println(maprFirstNameLastName.get("Afsal").get)

  }

}