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
	def product2(ns: List[Double]) = ns match {
		case Cons(0, _) => 0
		case _ => foldRight(ns, 1.0)(_ * _)
	}
	
	def length[A](as: List[A]): Int = foldRight(as, 0)((_, len) => 1 + len)

	@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
	}

	// use foldLeft
	def sum2(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
	def product3(ns: List[Double]) = ns match {
		case Cons(0, _) => 0
		case _ => foldLeft(ns, 1.0)(_ * _)
	}
	def length2[A](l: List[A]) = foldLeft(l, 0)((len, _) => len + 1)
  // implement reverse in term fo foldLeft
  def reverse[A](l: List[A]) = foldLeft(l, Nil:List[A])((a, b) => Cons(b,a))
 
  // cool functional programming!!!
	def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
		foldLeft(reverse(l), z)((b, a) => f(a, b))

	def foldRightViaFoldLeft2[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
		foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

	def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
		foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

	def append[A](l: List[A], r: List[A]): List[A] =
		foldRight(l, r)(Cons(_, _))

	def append1[A](l: List[A], r: List[A]): List[A] =
		foldLeft(l, (b: List[A]) => b)((g, a) => b => g(Cons(a, b)))(r)

	def concatenate[A](ll: List[List[A]]): List[A] = 
		foldRightViaFoldLeft(ll, Nil:List[A])(append)



}
