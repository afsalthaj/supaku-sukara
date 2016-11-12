package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.example.exercises.BankAccount
import org.specs2.Specification

/**
  * Created by afsalthaj on 12/11/2016.
  */
object BankAccountSpec extends Specification {
 def is =
   s"""
     | my bank account is stateful but isn't mutable $checkMutabilityOfBankAccount
   """.stripMargin

  def checkMutabilityOfBankAccount = {
    val s = BankAccount(1)
    s.deposit(1)
    assert(s.balance == 1)
  }
}
