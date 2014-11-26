object Module {
	// 2.1
	def fib(n: Int): Int = {
		@annotation.tailrec
		def loop(a: Int, b: Int, nth: Int): Int = {
			if (nth == 0) a
			else loop(b, a+b, nth - 1)
		}
		loop(0, 1, n)
	}
	// 2.2
	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		@annotation.tailrec
		def loop(nth: Int): Boolean = {
			if (nth >= as.length) true
			else if (!ordered(as(nth-1), as(nth))) false
			else loop(nth + 1)
		}
		loop(1)
	}
	// implementation for the example from the book
	def partial1[A,B,C](a: A, f: (A,B) => C): B => C = f(a, _)

	def uncurry[A,B,C](f: A => B => C): (A, B) => C = f(_)(_)

	def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
