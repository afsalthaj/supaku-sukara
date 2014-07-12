package com.scalabasics.scalaz

object IntMonoidX {
  
  def mappend (a:Int,b:Int) : Int= a +b
  def mzero : Int = 0
  
  

}

object MainFunction {
  def sum (x : List[Int]) : Int= x.foldLeft(IntMonoid.mzero){IntMonoid.mappend(_,_)}
  
  def main(args: Array[String]) {
  println(sum(List(1,2)))
}
  
}