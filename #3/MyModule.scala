/**
 * exercises solutions for fpinscala
 * @author vincent wei
 * @time 2014/10/22
 */

//package fpinscala.datastructures
	
sealed trait List[+A]
case object Nil extends List[Nothing] 
case class Cons[+A](h: A, t:List[A]) extends List[A] 

object List {

	def apply[A](as: A*):List[A] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail:_*))
	}

	def tail[A](l: List[A]) = l match {
		case Nil => sys.error("tail of empty list")
		case Cons(_, xs) => xs 
	}

	def setHead[A](l: List[A], h: A):List[A] = l match {
		case Nil => throw sys.error("empty list")
		case Cons(_, xs) => Cons(h, xs)
	}
	
	

	def drop[A](l: List[A], n: Int): List[A] = {
		if (n <= 0) l
		else l match {
			case Nil => Nil
			case Cons(_, xs) => drop(xs, n - 1)
		}
	}
		
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  	case Cons(h,t) if f(h) => dropWhile(t, f)
  	case _ => l
	}

	def init[A](l: List[A]): List[A] = l match {
		case Nil => throw sys.error("init of empty list")
		case Cons(_, Nil) => Nil
		case Cons(x, xs) => Cons(x, init(xs))
	}

	def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B):B = l match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}

	// use foldRight
	def sum(ns: List[Int]) = foldRight(ns, 0)(_ + _)
	def product(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
	// Exetcise 3.7 impossible


	// Exercise 3.9
	def length[A](as: List[A]): Int = foldRight(as, 0)((_, len) => 1 + len)
	// Exercise 3.10
	@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
	}
	// Exercise 3.11
	// use foldLeft
	def sum2(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
	def product3(ns: List[Double]) = ns match {
		case Cons(0, _) => 0
		case _ => foldLeft(ns, 1.0)(_ * _)
	}
	def length2[A](l: List[A]) = foldLeft(l, 0)((len, _) => len + 1)
  // Exercise 3.12 implement reverse in term fo foldLeft 
  def reverse[A](l: List[A]) = foldLeft(l, Nil:List[A])((a, b) => Cons(b,a))
 
  // cool functional programming!!!
  // Exercise 3.13
	def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
		foldLeft(reverse(l), z)((b, a) => f(a, b))

	def foldRightViaFoldLeft2[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
		foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

	def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
		foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
	//	Exercise 3.14
	def append[A](l: List[A], r: List[A]): List[A] =
		foldRight(l, r)(Cons(_, _))

	def append1[A](l: List[A], r: List[A]): List[A] =
		foldLeft(l, (b: List[A]) => b)((g, a) => b => g(Cons(a, b)))(r)
  // Exercise 3.15
	def concatenate[A](ll: List[List[A]]): List[A] = 
		foldRightViaFoldLeft(ll, Nil:List[A])(append)
	// Exercise 3.16
	def add1(l: List[Int]):List[Int] = 
		foldRight(l, Nil:List[Int])((x, y) => Cons(x + 1, y))
	// Exercise 3.17
	def map(l: List[Double]):List[String] = 
		foldRight(l, Nil:List[String])((x, y) => Cons(x.toString, y))
	// Exercise 3.18
	def map[A, B](l: List[A])(f: A => B) = 
		foldRight(l, Nil:List[B])((a, b) => Cons(f(a), b))
	// Exercise 3.19
	def filter[A](as: List[A])(f: A => Boolean): List[A] = 
		foldRight(as, Nil:List[A])((a, b) => if (f(a)) Cons(a, b) else b)
	// Exercise 3.20
	def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
		foldRight(as, Nil:List[B])((a, b) => append(f(a), b))
	// Exercise 3.21
	def filter2[A](l: List[A])(f: A => Boolean): List[A] = 
		flatMap(l)(x => if (f(x)) List(x) else Nil:List[A])
	// Exercise 3.22
	def addTwoList(l: List[Int], r: List[Int]):List[Int] = (l, r) match {
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addTwoList(xs, ys))
	}
	// Exercise 3.23
	def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C):List[C] = (l, r) match {
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
	}
	def take[A](l: List[A], n: Int): List[A] = 
		if (n <= 0) Nil
		else l match {
			case Nil => Nil
			case Cons(x, xs) => Cons(x, take(xs, n - 1))
		}

  def takeWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  	case Cons(x, xs) if (f(x)) => Cons(x, takeWhile(xs, f))
  	case _ => Nil
  }

  def forall[A](l: List[A], f: A => Boolean): Boolean = l match {
  	case Cons(x,xs) => if (!f(x)) false else forall(xs, f)
  	case Nil => true
  }
  
  def exists[A](l:List[A], f: A => Boolean): Boolean = l match {
  	case Cons(x, xs) => if (f(x)) true else exists(xs, f)
  	case Nil => false
  }
		
	// Exercise 3.24
	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
		def testEqual[A](a: List[A], b: List[A]): Boolean = (a, b) match {
			case (_, Nil) => true
			case (Cons(x, xs), Cons(y, ys)) if (x == y) => testEqual(xs, ys)
			case _ => false
		}
		if (testEqual(sup, sub)) true 
		else sup match {
			case Cons(_, xs) => hasSubsequence(xs, sub)
			case Nil => false
		}	
	}
	

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	// Exercise 3.25
	def size[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Branch(l, r) => size(l) + size(r) + 1
	}
	 // Exercise 3.26	
	def maximum(t: Tree[Int]): Int = t match {
		case Leaf(v) => v
		case Branch(l, r) => maximum(l) max maximum(r)
	}
	// Exercise 3.27	
	def depth[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Branch(l, r) => (depth(l) max depth(r)) + 1
	}
	// Exercise 3.28	
	def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Leaf(v) => Leaf(f(v))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}
	// Exercise 3.29
	def foldLeft[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
		case Leaf(v) => f(v)
		case Branch(l, r) => g(foldLeft(l)(f)(g), foldLeft(r)(f)(g))
		//g(foldLeft(r, foldLeft(l, b)(f, g))(f, g), t)
	}
	def size1[A](t: Tree[A]): Int = foldLeft(t)(x => 1)((l, r) => l + r + 1)
	def maximum1(t: Tree[Int]): Int = 
		foldLeft(t)(x => x)(_ max _)
	def depth1[A](t: Tree[A]): Int = 
		foldLeft(t)(x => 1)(_ max _)
	def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = 
		foldLeft(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
	def toString[A](t: Tree[A]): String = 
		foldLeft(t)(_.toString)("(" + _ +", " + _ + ")")	

}
