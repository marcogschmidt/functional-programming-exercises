package training

import scala.annotation.tailrec

object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Const[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) {
        Nil
      } else {
        Const(as.head, apply(as.tail: _*))
      }
    }

    def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Const(_, xs) => xs
    }

    def setHead[A](as: List[A], head: A): List[A] = as match {
      case Nil => Const(head, Nil)
      case Const(_, xs) => Const(head, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Const(_, xs) => if (n > 0) {
        drop(xs, n - 1)
      } else {
        l
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Const(a, as) => if (f(a)) {
        dropWhile(as, f)
      } else {
        Const(a, as)
      }
    }

  }

  def main(args: Array[String]): Unit = {

    val test = List(1, 2, 3, 4, 5)
    println(List.drop(test, 2))

    println(List.dropWhile(test, (x: Int) => x < 2))

  }


}
