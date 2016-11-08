package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State
import org.scalatest.WordSpec
import org.scalatest.prop.{Checkers, PropertyChecks}

class RngSpec extends WordSpec with PropertyChecks with Checkers {
  import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI._

  "RNG getDouble functions" must {
    "have passed" in {
      forAll { (w: Long) => {
        val s = double(SimpleRng(w))._1
        assert(s >= 0 && s < 1.0)
      }
      }
    }
  }

  "int double function" must {
    "have passed" in {
      forAll { (w: Long) => {
        val s = intDouble(SimpleRng(w))._1
        assert(s._1 >=0  && s._2 < 1.0)
      }
      }
    }
  }

 "The advanced rng function nonNegativeInt" must {
   import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIAdvanced._
   "have passed" in  {
     forAll { (n: (Long)) => {
       val s = nonNegativeInt(SimpleRng(n))
      assert(s._1 >= 0)
     }}
   }
 }

  "The advanced rng function nonNegativeLessThan" must {
    import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIAdvanced._
    "have passed" in  {
      forAll { (n: (Long)) => {
        // the reason behind the skewness is explained here.
        val s = nonNegativeLessThanSkewed(100)(SimpleRng(n))
        println(s._1)
        assert(s._1 <= 100)
      }}
    }
  }

  "The rng function created using generic state action map" must {
    import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State._
    "have passed" in  {
      forAll { (n: (Long)) => {
        val s: ((Double, Int), RNG) = doubleInt.map(t => (0.0, 0)).run(SimpleRng(n))
        assert(s._1 == (0.0, 0))
      }}
    }
  }

  // Property function doesnt suit
  "The rng function created using generic state action flatMap" must {
    import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State._
    "have passed" in  {
      forAll { (n: (Long)) => {
        val s: (Double, RNG) = doubleInt.map(t => (0.0, 0)).flatMap(t => State.unit(t._1)).run(SimpleRng(n))
        assert(s._1 == 0.0)
      }}
    }
  }
}