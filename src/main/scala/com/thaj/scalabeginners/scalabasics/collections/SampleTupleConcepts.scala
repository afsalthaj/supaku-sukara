package com.scalabasics.collections

/**
 * note the following features:
 * 1. default value of the fourth parameter in the SampleTupleConcepts class
 * 2. productArity function in Tuple
 * 3. different types of reference to a tuple
 */
class SampleTupleConcepts(val x1: String, val x2: Int, val x3: Int, val x4: Int = 4) {

  def convertToTuple() = (x1, x2, x3, x4)

}

object triggerTupleConversion {

  def main(args: Array[String]) {

    val instanceOfSampleTupleConcepts = new SampleTupleConcepts("afsal", 2, 3)

    val t = instanceOfSampleTupleConcepts.convertToTuple()

    println(t.productArity)

    val (t1, t2, t3, t4) = instanceOfSampleTupleConcepts.convertToTuple()

    println(t1 + "," + t4)

  }
}