package com.scalabasics.scalaz

trait Monoid[A]{
  
  def mappend(a:A,b:A):A
  def mzero:A
  
}

object IntMonoid extends Monoid[Int]{
  
  def mappend(a:Int,b:Int):Int=a+b
  def mzero : Int=0 
}

object StringMonoid extends Monoid[String] {
  def mappend(a:String,b:String):String="hai" +a+b
  def mzero : String="A"
}

object MainClass{
  
  implicit val intMonoid= IntMonoid
  implicit val stMonoid=StringMonoid
  def main(args: Array[String]) {
    sum(List(1,2,3))
    sum(List("afsal","afsdf")) 
} 
  def sum[A](a: List[A])(implicit m: Monoid[A]):A=a.foldLeft(m.mzero)(m.mappend)
}

