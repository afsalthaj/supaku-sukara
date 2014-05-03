package com.scalabasics.scalaimportstatements

class ImportStatements {

  def writeAboutBigInteger() = {

    import java.math.BigInteger.{ ONE => _, TEN, ZERO => JAVAZERO }
    //ONE is effectively undefined
    println()
    println(TEN)
    println(JAVAZERO)
  }

}

object UseImportStatements {

  def main(args: Array[String]) {
    val instacneOfAboveObj = new ImportStatements

    instacneOfAboveObj.writeAboutBigInteger
  }
}