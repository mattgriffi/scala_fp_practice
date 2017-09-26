/**
  * Created by MatthewG on 2017/09/07.
  */
object MyModule {
    def main(args: Array[String]): Unit = {
        System.out.println(formatResult(-42, "absolute value", abs))
        System.out.println(formatResult(12, "Fibonacci number", fibonacci))
        System.out.println(formatResult(7, "factorial", factorial))
        System.out.println(isSorted(Array(2, 4, 6, 7, 8), (x: Int, y: Int) => x < y))
        System.out.println(isSorted(Array(2, 9, 6, 7, 8), (x: Int, y: Int) => x < y))
        System.out.println(isSorted(Array(2, 4, 6, 7, 6), (x: Int, y: Int) => x < y))
        System.out.println(findFirst(Array(2, 3, 4, 5, 5, 6), (x: Int) => x == 5))
        System.out.println(findFirst(Array(2, 3, 4, 5, 5, 6), (x: Int) => x == 6))
        System.out.println(findFirst(Array(2, 3, 4, 5, 5, 6), (x: Int) => x == 2))
        System.out.println(findFirst(Array(2, 3, 4, 5, 5, 6), (x: Int) => x == 7))

        System.out.println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
        System.out.println(List.length(List(1, 2, 3, 4)))
        System.out.println(List.sum2(List(1, 2, 3, 4)))
        System.out.println(List.product2(List(1, 2, 3, 4)))
        System.out.println(List.length2(List(1, 2, 4, 5)))
        System.out.println(List.concatenate(List(List(1, 2, 3), List(5, 6, 7), List(11, 12, 13))))
        System.out.println(List.plus1(List(1, 2, 3, 4, 5)))
        System.out.println(List.map(List(1, 2, 3, 4, 5))(_ * 2))
        System.out.println(List.filter(List(1, 2, 3, 4, 5, 6, 7, 8, 9))(x => x % 2 == 0))
        System.out.println(List.flatMap(List(1, 2, 3, 4, 5))(x => List(x, 2*x, 3*x)))

    }

    def abs(n: Int): Int = {
        if (n < 0) -n
        else n
    }

    def factorial(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, acc: Int): Int = {
            if (n <= 1) acc
            else loop(n - 1, n * acc)
        }
        loop(n, 1)
    }

    def fibonacci(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, prev: Int, curr: Int): Int = {
            if (n < 1) prev
            else loop(n - 1, curr, prev + curr)
        }
        loop(n, 0, 1)
    }

    def formatResult(n: Int, name: String, f: Int => Int): String = {
        val formatString = "The %s of %d is %d"
        formatString.format(name, n, f(n))
    }

    def findFirst[A](a: Array[A], key: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int = {
            if (n >= a.length) -1
            else if (key(a(n))) n
            else loop(n + 1)
        }
        loop(0)
    }

    def isSorted[A](a: Array[A], ordered: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean = {
            if (n >= a.length) true
            else if (ordered(a(n-1), a(n))) loop(n + 1)
            else false
        }

        if (a.length <= 1) true
        else loop(1)
    }

    def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
        (b: B) => f(a, b)
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
        (a: A) => (b: B) => f(a, b)
    }

    def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
        (a: A, b: B) => f(a)(b)
    }

    def compose[A, B, C](f: B => C, g: A => B): A => C = {
        (a: A) => f(g(a))
    }
}
