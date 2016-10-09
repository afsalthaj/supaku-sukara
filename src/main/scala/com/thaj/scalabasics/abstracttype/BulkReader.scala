package com.scalabasics.abstracttype

import java.io.File
import java.io.BufferedInputStream
import java.io.FileInputStream

// an abstract class which declares a field and a function; and defines another function
abstract class BulkReader {

  val source: Any
  def read: String
  def printFile: Unit = {
    println(read)
  }

}
//a class that extends the abstract class BulkReader
class StringBulkReader(val source: String) extends BulkReader {

  // defines the function read. the function returns a string; the value of 'source'
  def read = source

}

// another class that extends the abstract class BulkReader
class FileBulkReader(val source: File) extends BulkReader {

  // defines the function read. it reads a text file and returns the whole content
  def read = {
    val in = new BufferedInputStream(new FileInputStream(source))

    val numBytes = in.available()
    val bytes = new Array[Byte](numBytes)
    in.read(bytes, 0, numBytes)

    new String(bytes)

  }

}

// main function that makes use of the two classes. note that the types in constructors are different since it is "Any" in abstract class
object UseThis {

  def main(args: Array[String]) {

    val x = new FileBulkReader(new File("src/com/scalabasics/abstracttype/BulkReader.scala"))
    val y = new StringBulkReader("hai scala")

    x.printFile
    println("-----------------------")
    y.printFile
  }
}