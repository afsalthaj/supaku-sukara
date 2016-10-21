package com.thaj.functionalprogramming.example

import org.specs2.Specification

/**
  * Created by afsalthaj on 21/10/2016.
  */
object OptionSpec extends Specification {
import com.thaj.functionalprogramming.example.exercises.OptionOperation._
  def is =
    s"""
      |${ Some("afsa").filter(_ == "afa") == None  }
    """.stripMargin

}
