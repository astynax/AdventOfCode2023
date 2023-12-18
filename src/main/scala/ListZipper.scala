package me.astynax

import scala.annotation.tailrec

case class ListZipper[A] private (position: Int,
                                  private val ls: List[A],
                                  focus: A,
                                  private val rs: List[A]) {

  def back: Option[ListZipper[A]] = ls match {
    case l :: rest => Some(ListZipper(position - 1, rest, l, focus :: rs))
    case _ => None
  }

  def forward: Option[ListZipper[A]] = rs match {
    case r :: rest => Some(ListZipper(position + 1, focus :: ls, r, rest))
    case _ => None
  }

  def first: ListZipper[A] = back.map { l => l.first }.getOrElse(this)

  def last: ListZipper[A] = forward.map { r => r.last }.getOrElse(this)

  def toList: List[A] = {
    val l = first
    l.focus :: l.rs
  }

  def iterate: Iterator[ListZipper[A]] = Iterator
    .unfold(Option.apply(this))(_.map { z => z -> z.forward })

  def findForward(test: A => Boolean): Option[ListZipper[A]] =
    iterate.find { z => test(z.focus) }

  def find(test: A => Boolean): Option[ListZipper[A]] = first.findForward(test)
}

object ListZipper {
  def fromList[A]: List[A] => Option[ListZipper[A]] = {
    case x :: xs => Some(ListZipper(0, List(), x, xs))
    case _ => None
  }
}
