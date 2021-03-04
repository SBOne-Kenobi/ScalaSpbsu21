package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._


sealed trait MyGenericList[+T] {
  def head: T

  def tail: MyGenericList[T]

  def drop(n: Int): MyGenericList[T]

  def take(n: Int): MyGenericList[T]

  def map[H](f: T => H): MyGenericList[H]

  def ::[U >: T](elem: U): MyGenericList[U] = new ::(elem, this)
}

final case class ::[+T](elem: T, list: MyGenericList[T]) extends MyGenericList[T] {
  override val head: T = elem

  override val tail: MyGenericList[T] = list

  override def drop(n: Int): MyGenericList[T] = if (n == 0) this else tail.drop(n - 1)

  override def take(n: Int): MyGenericList[T] = if (n == 0) MyNil else head :: tail.take(n - 1)

  override def map[H](f: T => H): MyGenericList[H] = f(head) :: tail.map(f)
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef

  override def tail: MyGenericList[Nothing] = undef

  override def drop(n: Int): MyGenericList[Nothing] = if (n == 0) this else undef

  override def take(n: Int): MyGenericList[Nothing] = if (n == 0) this else undef

  override def map[H](f: Nothing => H): MyGenericList[H] = MyNil
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[T](seq: Seq[T]): MyGenericList[T] =
    seq.foldRight[MyGenericList[T]](MyNil)((v: T, acc: MyGenericList[T]) => v :: acc)

  def sum(list: MyGenericList[Int]): Int = list match {
    case MyNil => undef
    case _ => foldLeft[Int, Int](0)(_ + _)(list)
  }

  def size(list: MyGenericList[Any]): Int = foldLeft(0)((acc: Int, _: Any) => acc + 1)(list)

  // extra task: implement sum using foldLeft
  def foldLeft[T, H](ini: H)(op: (H, T) => H): MyGenericList[T] => H = {
    case MyNil => ini
    case x :: xs => foldLeft(op(ini, x))(op)(xs)
  }
}
