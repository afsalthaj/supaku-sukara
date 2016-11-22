package com.thajaf.fp.example.wrong.fp

/**
 * An ugly code base that doesn't show any value of FP.
 * buyCofee has a side-effect of the whole process of charging a creditcard
 * Also, we can't multiple coffee without multiple credit card processing charge.
 */
private case class CreditCard (cNumber: String) {

  def charge(price: Int) = println(s"credit card $cNumber charged Rs.$price")
}

sealed trait Beverage {
  def price: Int
}

class Coffee extends Beverage {
  val price = 25
}

class Tea extends Beverage {
  val price = 10
}

class Cafe {

  def buyCofee (cc: CreditCard): Coffee = {

    val cup = new Coffee

    cc.charge(cup.price)

    cup
  }
}

