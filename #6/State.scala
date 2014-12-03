package fpinscala.chapter_6

trait RNG {
	def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long)extends RNG {
	def nextInt: (Int, RNG) = {
		val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
		val nextRNG = SimpleRNG(newSeed)
		val n = (newSeed >>> 16).toInt
		(n, nextRNG)
	}
}

object RNG {
	def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (v, s) = rng.nextInt
		(if (v < 0) -(v + 1) else v, s)
	}

	def double(rng: RNG): (Double, RNG) = {
		val (v, s) = nonNegativeInt(rng)
		(v.toDouble / Int.MaxValue.toDouble + 1, s)
	}

	def intDouble(rng: RNG): ((Int,Double), RNG) = {
		val (v1, s1) = rng.nextInt
		val (v2, s2) = double(s1)
		((v1, v2), s2)
	}

	def doubleInt(rng: RNG): ((Double,Int), RNG) = {
		val (v1, s1) = double(rng)
		val (v2, s2) = s1.nextInt
		((v1, v2), s2)
	}

	def double3(rng: RNG): ((Double,Double,Double), RNG) = {
		val (v1, s1) = double(rng)
		val (v2, s2) = double(s1)
		val (v2, s3) = double(s2)
		((v1, v2, v3), s3)
	}

	def ints(count: Int)(rng: RNG) = {
		def loop(cnt: Int, acc: List[Int], s: RNG) = 
			if (cnt == 0) (acc, s)
			else {
				val (v, s) = s.nextInt
				loop(cnt - 1, v :: acc, s)
			}
		loop(count, Nil, rng)
	}
	// factor common pattern
	type Rand[+A] = RNG => (A, RNG)

	def unit[A](a: A): RNG[A] = rng => (a, rng)

	def map(s: Rand[A])(f: A => B): Rand[B] = 
		rng => {
			val (v, s) = s(rng)
			(f(v), s)
		}
	// fun functional programming
	def nonNegativeEven: Rand[Int] = 
		map(nonNegativeInt)(x => x - x % 2)

	def double_1: Rand[Double] =
		map(nonNegativeInt)(_ % Int.MaxValue.toDouble + 1.0)

	def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
		rng => {
			val (v1, s1) = ra(rng)
			val (v2, s2) = ra(s1)
			(f(v1, v2), s2)
		}

	def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
		map2(ra, rb)((_, _))
	// amazing
	val randIntDouble: Rand[(Int, Double)] =
		both(ints, double)
	val randDoubleInt: Rand[(Double, Int)] =
		both(double, ints)

	def sequence_1[A](fs: List[Rand[A]]): Rand[List[A]] = 
		rng => fs.foldRight((Nil: List[A], rng))((a, b) => {
			val (v, s) = a(b._2)
			(v :: b._1, s)
			})
	// a more concise version
	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
		fs.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))

}