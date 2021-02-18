package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
    def i: Int

    def head: Int = i

    def tail: IntList

    def drop(n: Int): IntList

    def take(n: Int): IntList

    def map(f: Int => Int): IntList

    def ::(elem: Int): IntList = new ::(elem, this)
}

case class ::(override val i: Int, override val tail: IntList) extends IntList {
    override def drop(n: Int): IntList = if (n == 0) this else tail.drop(n - 1)

    override def take(n: Int): IntList = if (n == 0) IntNil else i :: tail.take(n - 1)

    override def map(f: Int => Int): IntList = f(i) :: tail.map(f)
}

case object IntNil extends IntList {
    override def i: Int = undef

    override def tail: IntList = undef

    override def drop(n: Int): IntList = if (n == 0) this else undef

    override def take(n: Int): IntList = if (n == 0) this else undef

    override def map(f: Int => Int): IntList = IntNil
}

object IntList {
    def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

    def fromSeq(seq: Seq[Int]): IntList = seq.foldRight[IntList](IntNil)((v: Int, acc: IntList) => v :: acc)

    def sum(intList: IntList): Int = intList match {
        case IntNil => undef
        case _ => foldLeft(0)(_ + _)(intList)
    }

    def size(intList: IntList): Int = foldLeft(0)((acc: Int, _: Int) => acc + 1)(intList)

    // extra task: implement sum using foldLeft
    def foldLeft[a](ini: a)(op: (a, Int) => a): IntList => a = {
        case IntNil => ini
        case x :: xs => foldLeft(op(ini, x))(op)(xs)
    }
}
