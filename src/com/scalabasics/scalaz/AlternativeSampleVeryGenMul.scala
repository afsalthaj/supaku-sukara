package com.scalabasics.scalaz

object AlternativeSampleVeryGenMul {
  
  def mul[A:GenericMultiplier](a:A,b:A)={
    
    val m=implicitly[GenericMultiplier[A]]
    m.mul(a, b) 
    
  }
  

}