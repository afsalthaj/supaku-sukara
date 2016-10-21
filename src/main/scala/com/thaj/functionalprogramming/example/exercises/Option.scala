package com.thaj.functionalprogramming.example.exercises

/**
  * Created by afsalthaj on 20/10/2016.
  */

object OptionOperation {

  sealed trait Option[+A] {
    // a handy get function
    def get: A

    def isEmpty: Boolean

    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      if (isEmpty) None else f(this.get)

    // We know Nothing is a subtype of everthing.
    // this is why val s: List[String] = Nil works.
    // Nil means A is Nothing. hence if A is a subtype of B,
    // then Option[A] is a subtype of Option[B]
    // in order to keep variance B is a super type of A
    // or , A must be equal to B, or B must be greater than A.
    // Somethhg like Lion should be tagged as a Lioj, or atleast
    // tagged as Animal. Also default will get executed only if the condition is valid
    def getOrElse[B >: A](default: => B): B = if (isEmpty) default else this.get

    // If the option is empty then I need another option (this is not getOrElse).. It is orElse
    def orElse[B >: A](ob: => Option[B]): Option[B] = if (isEmpty) ob else this

    //filter
    def filter(f: A => Boolean): Option[A] = if (isEmpty || f(this.get)) this else None
  }

  case class Some[A](get: A) extends Option[A] {
    def isEmpty = false
  }

  case object None extends Option[Nothing] {
    def isEmpty: Boolean = true

    def get = throw new NoSuchElementException("None.get")
  }

}