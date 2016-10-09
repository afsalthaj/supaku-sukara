package com.scalabasics.collections

import java.util.HashMap
/**
 * class discuss the map in scala, and concept of Option class
 * Some and None are subclasses of Option. Instead of returning a null,
 * which is a keyword when user expects an object as in Java,
 * we get an object(value) which is either wrapped in Some or None
 */
object SampleSomeNone {

  def main(args: Array[String]) {

    val mapInstance = Map("US" -> "Columbus", "India" -> "Delhi", "Japan" -> "Tokyo")

    println(mapInstance.get("US"))
    println(mapInstance.get("Europe"))
    //getorElse method returns either the value in the Option if it is a Some instance or it returns the second argument if it is a None instance
    println(mapInstance.get("Europe").getOrElse("OOPS where is it"))
    println(mapInstance.get("Europe").getOrElse(mapInstance.get("Europe").exists((s: String) => true)))
    println(mapInstance.get("Europe").getOrElse(mapInstance.get("US").exists((s: String) => true) + " for US, India and Japan only and false for Europe"))
  }

}