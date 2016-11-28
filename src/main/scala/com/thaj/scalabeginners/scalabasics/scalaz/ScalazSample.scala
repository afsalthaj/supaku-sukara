package com.scalabasics.scalaz


//sub typing for add function

object ScalazSample {

  def main(args: Array[String]) {
    
  println(  head(Car("Civic") :: Car("CR-V") :: Nil))


  }

  def head[A](xs: List[A]): A = xs(0)
  



}

case class Car(make: String)


