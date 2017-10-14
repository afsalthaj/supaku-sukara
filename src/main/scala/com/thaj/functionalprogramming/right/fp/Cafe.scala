package com.thajaf.fp.example.right.fp

// Wrapping the whole complexity of credit card and price to be charged as `Charge`
case class Charge(creditCard: CreditCard, price: Int) {
  // combining credit card is the responsibility of Charge object
  def +(other: Charge) =
    if (this.creditCard == other.creditCard)
      Charge(creditCard, this.price + other.price)
    else
      throw new Exception("Credit Cards are different")

  // receive money is the responsibility of charge object since it has credit card information
  def receiveMoney = creditCard.chargeCreditCard(this)
}

// Credit Card
case class CreditCard(cNumber: String) {
  // The complex mechanism of charge happens here
  def chargeCreditCard(charge: Charge) = println(s"Credit Card $cNumber charged price ${charge.price}")
}

// Beverage
sealed trait Beverage { def price: Int }
// Coffee
class Coffee extends Beverage { val price = 25 }
// Tea
class Tea extends Beverage { val price = 10 }

// The real Cafe, handling buying Coffee
class Cafe {
  // The complex mechanism of charge is not called anywhere, hence no side effect
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee
    (cup, Charge(cc, cup.price))
  }

  // The complex mechanism of charge is not called anywhere, hence no side effect
  def buyNCoffee(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val coffee = List.fill(n)(buyCoffee(cc))
    val unZipped: (List[Coffee], List[Charge]) = coffee.unzip

    (unZipped._1, unZipped._2.reduce(_ + _))
  }

  // abstracted away the complexity charging the credit card
  private def receiveMoney(c: List[Charge]) = c.reduce(_ + _).receiveMoney
}
