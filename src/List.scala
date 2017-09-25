/**
  * Created by MatthewG on 2017/09/15.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B) : B = as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum(ints: List[Int]): Int = {
        foldRight(ints, 0)(_ + _)
    }

    def sum2(ints: List[Int]): Int = {
        foldLeft(ints, 0)(_ + _)
    }

    def product(ds: List[Double]): Double = {
        foldRight(ds, 1.0)(_ * _)
    }

    def product2(ds: List[Double]): Double = {
        foldLeft(ds, 1.0)(_ * _)
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

    def length[A](as: List[A]): Int = {
        foldRight(as, 0)((_, x) => x + 1)
    }

    def length2[A](as: List[A]): Int = {
        foldLeft(as, 0)((x, _) => x + 1)
    }
}
