package com.scalabasics.scalaz


trait GenericMultiplier[A]{
  
  def mul(a:A,b:A) :A
  
}
object SampleMulVeryGeneric  {
  
  implicit val multiplier=MultiplierInt

   def main(args: Array[String]) {
   mul(1,2)
}
  def mul[A](a:A,b:A)(implicit gm: GenericMultiplier[A])=gm.mul(a, b)
  
}

object MultiplierInt extends GenericMultiplier[Int]{
  def mul(a:Int,b:Int) :Int =a
}