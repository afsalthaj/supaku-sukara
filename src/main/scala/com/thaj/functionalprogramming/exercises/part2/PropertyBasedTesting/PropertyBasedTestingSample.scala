package com.thaj.functionalprogramming.example.exercises.part2


object Sample {
  /*
    {{{
       // A generator of list of integers between 0 and 100
        val intList: Gen[List[Int]] = Gen.listOf(Gen.choose(0, 100))
        val prop = forAll(intList)(ns => ns.reverse.reverse == ns) &&
         forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
        val failingProp = forAll(intList)(ns = ns.reverse == ns
      )

      > prop.check
      > failingProp.check
    }}}
    intList is a Gen[List[Int]]. We can sample from this generator,
    and it will produce lists of different lengths, filled with random
    numbers between 0 and 100. Generators in property based testing has a rich
    API. We can combine and compose generators in different ways, reuse them,
    and so on.
    Like generator, forAll returns a property. Like generators, properties
    can also have a rich API. We combined two properties using &&.

    // Exercise 8.1
    Refer test cases
   */
}