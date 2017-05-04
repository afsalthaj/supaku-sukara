package com.thaj.functionalprogramming.layoutlibrary

/**
  * Created by afsalthaj on 5/05/2017.
  */
abstract class Element {
  import Element._
  def contents: Array[String]

  def width: Int = contents(0).length

  def height: Int = contents.length

  override def toString = contents mkString "\n"

  def above(that: Element) = {
    val this1 = this stabiliseWidth that.width
    val that1 = that stabiliseWidth this.width
    elem (this1.contents ++ that1.contents)
  }

  def beside(that: Element): Element = {
    val this1 = this stabiliseHeight that.height
    val that1 = that stabiliseHeight this.height

    elem(for ((line1, line2) <- this1.contents zip that1.contents)
      yield line1 ++ line2)
  }

  def stabiliseWidth(w: Int): Element = {
     if (w <= width) this
     else {
       val left = Element.elem(' ', (w - width)/2 , height)
       val right = Element.elem (' ', (w - width - left.width), height)
       left beside this beside right
     }
  }

  def stabiliseHeight(h: Int): Element = {
     if (h <= height) this
     else {
       val top = elem(' ', width, (h - height)/2)
       val bottom = elem(' ', width, h - height - top.height)
       top above this above bottom
     }
  }

}

object Element {
  private class ArrayElement(val contents: Array[String]) extends Element
  private class UniformElement(char: Char, width: Int, height: Int) extends Element {
    def contents = Array.fill(height)(char.toString * width)
  }

  private class LineElement(s: String) extends Element {
    val contents = Array(s)
    override val width : Int= s.length
    override val height: Int  = 1
  }

  def elem(array: Array[String]) = new ArrayElement(array)

  def elem(ch: Char, width: Int, height: Int) = new UniformElement(ch, width, height)

  def elem(line: String) = new LineElement(line)
}
