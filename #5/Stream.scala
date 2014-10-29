sealed trait Stream[+A] {

	def toList: List[A] = this match {
		case Cons(h, t) => h() :: t().toList
		case _ => List()
	}

	def take(n: Int): Stream[A] = {
		if (n <= 0) Empty
		else this match {
			case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
			case _ => Stream.empty
		}
	}

	def drop(n: Int): Stream[A] = {
		if (n <= 0) this
		else this match {
			case Empty => Stream.empty
			case Cons(h, t) => t().drop(n - 1)
		}
	}

	def takeWhile(p: A => Boolean): Stream[A] = this match {
		case Cons(h, t) if (p(h())) => Stream.cons(h(), t().takeWhile(p))
		case _ => Stream.empty
	}
	
	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
		case Cons(h, t) => f(h(), t().foldRight(z)(f))
		case _ => z
	}

	def exists(p: A => Boolean): Boolean =
		foldRight(false)((a, b) => p(a) || b)

	def forAll(p: A => Boolean): Boolean = 
		foldRight(true)((a, b) => p(a) && b)

	def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = 
		foldRight(Stream(): Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

	def headOption: Option[A] = 
		foldRight(None: Option[A])((a, b) => Some(a))

	def map[B](f: A => B): Stream[B] = 
		foldRight(Stream(): Stream[B])((a, b) => Stream.cons(f(a), b))

	def filter(p: A => Boolean): Stream[A] = 
		foldRight(Stream(): Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

	def append[B >: A](s: Stream[B]): Stream[B] =
		foldRight(s)((a, b) => Stream.cons(a, b))

	def flatMap[B](f: A => Stream[B]): Stream[B] =
		foldRight(Stream(): Stream[B])(f(_) append _)

	def mapViaUnfold[B](f: A => B): Stream[B] = 
		Stream.unfold(this)((sm) => sm.headOption map (a => (f(a), sm.drop(1))))

	def takeViaUnfold(n: Int): Stream[A] =
		Stream.unfold((this, n))( _ match {
			case (sm, num) => 
			if (num <= 0) None: Option[(A, (Stream[A], Int))]
			else sm.headOption map (a => (a, (sm.drop(1), num - 1)))
		})

	def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
		Stream.unfold(this)(sm => sm.headOption flatMap (a => if (p(a)) Some(a, sm.drop(1)) else None: Option[(A, Stream[A])]))

	def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
		Stream.unfold((this, s))(_ match {
			case (l, r) => for (lh <- l.headOption; rh <- r.headOption) yield (f(lh, rh), (l.drop(1), r.drop(1)))
		})

	def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
		Stream.unfold((this, s2))(_ match {
			case (a, b) => 
			(a.headOption, b.headOption) match {
			case (None, None) => None: Option[((Option[A], Option[B]), (Stream[A], Stream[B]))]
			case (l, r) => Some(((l, r), (a.drop(1), b.drop(1)))) 
		}})
  // Exercise 5.14
	def startsWith[A](s: Stream[A]): Boolean =
		zipAll(s).takeWhile(_._2 != Empty).forAll(a => a._1 == a._2)

	// Exercise 5.15
	def tails: Stream[Stream[A]] =
		Stream.unfold(this)((a) => a match {
			case Cons(h, t) => Some((a, t()))
			case Empty => None: Option[(Stream[A], Stream[A])] 
		}
		).append(Stream.empty[Stream[A]])
	// Awesome!!!
	def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)
	// Exercise 5.16
	def scanRight[B](z: B)(f: (A, =>B) => B): Stream[B] = 
		foldRight((z, Stream(z)))((a, b) => (f(a, b._1), Stream.cons(f(a, b._1), b._2)))._2


	def tailsViaScanRight: Stream[Stream[A]] = 
		scanRight(Stream.empty: Stream[A])(Stream.cons(_, _))
	



}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
	// closure, refer to page 199 in "Programming in scala", 2nd Edition5
	def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
		lazy val head = h
		lazy val tail = t
		Cons(() => head, () => tail)
	}

	def empty[A]: Stream[A] = Empty

	def apply[A](as: A*): Stream[A] = {
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail:_*))
	}
	
	def constant[A](a: A): Stream[A] = cons(a, constant(a))

	def from(n: Int): Stream[Int] = cons(n, from(n + 1))

	def fibs: Stream[Int] = {
		def go(x: Int, y: Int): Stream[Int] = {
			cons(x, go(y, x + y))
		}
		go(0, 1)
	}

	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
		val r: Option[(A, S)] = f(z)
		r match {
			case None => empty[A]
			case Some((v, s)) => cons(v, unfold(s)(f))
		}
	}

	def fibsViaUnfold: Stream[Int] = unfold((0, 1))(a => Some((a._1, (a._2, a._1 + a._2))))

	def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(a => Some((a, n + 1)))
	
	def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

	def onesViaUnfold: Stream[Int] = unfold(1)(a => Some(1, 1))

}