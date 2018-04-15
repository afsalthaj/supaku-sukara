/*
package com.thaj.functionalprogramming.exercises.part2.parsercombinator

/**
 * u might well be thinking why could it be?
 * case class Parser[A](run: A => Either[ParseError, A]) {
 *   def unit[A](a: A): Parser[A] = Parser[A](_ => Right(a))
 *   def map[A](f: A => B): Parser[A] = Parser(a => unit(f(a))
 * }
 * But let's follow the starting point of the text book.
 * U can consider this trait as an algebraic API that exposes a functions
 * that may be built of a few primitive combinators. Try to have a few primitive
 * combinators that do the most trivial things. You will then start thinking of
 * combinators to unlock your library.
 */
trait Parsers[Parser[_], ParserError] { self =>
  def run[A](parser: Parser[A])(input: String): Either[ParserError, A]

  // A string can be converted to a Parser[String] automatically
  implicit def string(s: String): Parser[String]
  // A parse[String] can be converted to ParserOps that is defined with more combinators
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  // A A can be first converted to Parser[String] and then to ParserOps that has more combinators
  // This is only a starting point. You can design it in your own way.
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  // Char can be defined in terms of string, with the help of the combinator map
  def char(c: Char) = string(c.toString).map(_.charAt(0))
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def many[A](c: Parser[A]): Parser[List[A]]
  // How to get a parser that returns the size of the repeated characters
  val numA: Parser[Int] = char('a').many.map(_.size)
  /**
   *  A Parser[Int] that recognizes one or more 'a' characters, and whose result value is the number of 'a' characters it has seen. (Is this defined somehow in terms of the same combinators as the parser for 'a' repeated zero or more times?) The parser should fail when given a string without a starting 'a'. How would you like to handle error reporting in this case? Could the API support giv- ing an explicit message like "Expected one or more 'a'" in the case of failure?
   */
  val oneOrMoreA: Parser[Int] = ??? //numA.map(x => if x == 0 )/// Stuck

  /**
   * A parser that recognizes zero or more 'a', followed by one or more 'b', and which results in the pair of counts of characters seen. For instance, given "bbb", we get (0,3), given "aaaab", we get (4,1), and so on.
   */

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def product[A](a: Parser[A], b: Parser[A]): Parser[(A, A)] = ???
  // may be
  val numAorB = product(char('a').many.map(_.size), char('b').many.map(_.size))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
  }
}

*/
