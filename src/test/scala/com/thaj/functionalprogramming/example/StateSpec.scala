package com.thaj.functionalprogramming.example

import org.scalatest.FunSuite

/**
  * Created by afsalthaj on 13/11/2016.
  */
class StateSpec extends FunSuite {
  import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI.SimpleRng
  import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State._

  test("intsRand") {
    // Yes, to make sure all these follows substitutional model with referential transparency
    assert(ints(3).run(SimpleRng(42L)) === ints(3).run(SimpleRng(42L)))
  }

}
