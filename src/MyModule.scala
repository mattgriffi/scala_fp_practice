/**
  * Created by MatthewG on 2017/09/07.
  */
object MyModule {
    def main(args: Array[String]): Unit = {
        System.out.println(formatResult(-42, "absolute value", abs))
        System.out.println(formatResult(12, "Fibonacci number", fibonacci))
        System.out.println(formatResult(7, "factorial", factorial))
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
}
