package com.thaj.functionalprogramming.example.exercises

/**
 * Created by afsalthaj on 12/11/2016.
 */

/**
 * {{{
 * class BankAccount {
 *
 * private var bal: Int = 0
 *
 * def balance: Int = bal
 *
 * def deposit(amount: Int) {
 * require(amount > 0)
 * bal += amount
 * }
 *
 * def withdraw(amount: Int): Boolean =
 * if (amount > bal) false
 * else {
 * bal -= amount
 * true
 * }
 * }
 * }}}
 */
// A very simple stateful API. However stateful APIs may not be as simple
// as what is represnted below in real life. A stateful API always deal with
// a type A and a new state S. That is, given a state S, it gives you A and the new state of S.
//  Below example S and A are predefined and they are
// `Int` and `BankAccount`.
case class BankAccounts(balance: Int) {
  def deposit(amount: Int) = {
    require(amount > 0)
    val newBalance = this.balance + amount
    BankAccounts(newBalance)
  }

  def withDraw(amount: Int) = {
    require(amount > 0)
    val newBalance = this.balance - amount
    BankAccounts(newBalance)
  }

  // To add a chain of changes to bank account
  // The stateful API is simple enough and composition of state actions is simpler too
  //val bankAccount = BankAccounts(10)
  //val stateFulCompositions = bankAccount.deposit(10).deposit(100).withDraw(5).deposit(100)
}
