/**
  * Created by MatthewG on 2017/09/15.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1
        case Cons(0.0, _) => 0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

    def tail[A](xs: List[A]): List[A] = xs match {
        case Nil => Nil
        case Cons(_, x) => x
    }

    def setHead[A](xs: List[A])(head: A): List [A] = xs match {
        case Nil => List(head)
        case Cons(_, x) => Cons(head, x)

    }

    def drop[A](xs: List[A], n: Int): List[A] = xs match {
        case Cons(_, x) if n > 0 => drop(x, n - 1)
        case _ => xs
    }

    def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
        case Cons(h, t) if f(h) => dropWhile(t)(f)
        case _ => as
    }

    def append[A](a1: List[A])(a2: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t)(a2))
    }

    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }
}
