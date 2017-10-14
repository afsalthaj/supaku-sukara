package com.thaj.scalabeginners.scalabasics.queue

/**
 * Created by afsalthaj on 7/05/2017.
 */

class Fruit

class Apple extends Fruit

class Orange extends Fruit

// Refer chapter 19 page no: 402
object Test {
  val queue: Queue[Fruit] = Queue(new Apple, new Apple)
  def testingQueue: Queue[Fruit] = queue.enqueue(new Orange)
}

trait Queue[T] {
  def head: T
  def tail: Queue[T]
  /**
   * if it was T directly, covariant Type T occurs in negative position comes in picture
   * @param x
   * @return
   */
  def enqueue[U >: T](x: U): Queue[U]
}

object Queue {

  def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList, Nil)

  class QueueImpl[T](private val leading: List[T], private val trailing: List[T]) extends Queue[T] {
    def mirror =
      if (leading.isEmpty)
        new QueueImpl(trailing.reverse, Nil)
      else
        this

    def head = mirror.leading.head

    def tail = {
      val q = mirror
      new QueueImpl[T](q.leading, q.trailing)
    }

    def enqueue[U >: T](x: U) = new QueueImpl[U](leading, x :: trailing)
  }
}
