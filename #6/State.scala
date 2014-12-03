//package fpinscala

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
		val (v3, s3) = double(s2)
		((v1, v2, v3), s3)
	}

	def ints(count: Int)(rng: RNG) = {
		def loop(cnt: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = 
			if (cnt == 0) (acc, rng)
			else {
				val (v, s) = rng.nextInt
				loop(cnt - 1, v :: acc, s)
			}
		loop(count, Nil, rng)
	}
	// factor common pattern
	type Rand[+A] = RNG => (A, RNG)

	def unit[A](a: A): Rand[A] = rng => (a, rng)

	def map[A, B](rand: Rand[A])(f: A => B): Rand[B] = 
		rng => {
			val (v, s) = rand(rng)
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
			val (v2, s2) = rb(s1)
			(f(v1, v2), s2)
		}

	def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
		map2(ra, rb)((_, _))
	// amazing
	val randIntDouble: Rand[(Int, Double)] =
		both(_.nextInt, double)
	val randDoubleInt: Rand[(Double, Int)] =
		both(double, _.nextInt)

	def sequence_1[A](fs: List[Rand[A]]): Rand[List[A]] = 
		rng => fs.foldRight((Nil: List[A], rng))((a, b) => {
			val (v, s) = a(b._2)
			(v :: b._1, s)
			})
	// a more concise version
	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
		fs.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))
    
  def nonNegativeLessThan(n: Int): Rand[Int] = rng => {
    val (v, s) = nonNegativeInt(rng)
    val mod = v % n
    if (v - mod + n - 1 > 0) (v, s) else nonNegativeLessThan(n)(s) 
  }
  
 def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
   val (a, s)  = f(rng)
   g(a)(s) 
 } 
 
 def map_1[A, B](rand: Rand[A])(f: A => B): Rand[B] = 
   flatMap(rand)(a => unit(f(a)))
   
 def map2_2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
   flatMap(ra)(a => map(rb)(b => f(a, b)))
}
   
import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    flatMap(a => sb map (b => f(a, b)))
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = 
    State(s => (a, s))
  
  def sequenceViaFoldRight[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](Nil: List[A]))((a, b) => a.map2(b)(_ :: _))
    
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }
 
  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit(Nil: List[A]): State[S, List[A]])((b, a) => a.map2(b)(_ :: _))
   
  def get[S]: State[S, S] = State(s => (s, s))
  def get_1[S]: State[S, S] = State(s => (s, s))
  
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def set_1[S](s: S): State[S, Unit] = State(_ => ((), s))
  
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
  // using  flatMap
  def modify_1[S](f: S => S): State[S, Unit] = 
  	get flatMap (s => set(f(s)))
  
}
//
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    })))
    s <- get
  } yield (s.coins, s.candies)
}
