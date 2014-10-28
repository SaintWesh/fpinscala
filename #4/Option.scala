sealed trait Option[+A] {

	def map[B](f: A => B): Option[B] = this match {
		case None => None
		case Some(a) => Some(f(a))
	}
	
	def flatMap[B](f: A => Option[B]): Option[B] = this match {
		case None => None
		case Some(a) => f(a)
	}

	def flatMap1[B](f: A => Option[B]): Option[B] =
		map(f) getOrElse None
	
	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(a) => a
	}
	
	def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
		case None => ob
		case _ => this
	}

	def orElse1[B >: A](ob: => Option[B]): Option[B] =
		map(Some(_)) getOrElse ob

	def filter(f: A => Boolean): Option[A] = this match {
		case None => None
		case Some(a) => if (f(a)) this else None
	}

	def filter1(f: A => Boolean): Option[A] =
		flatMap(a => if (f(a)) Some(a) else None)
	
}
//sealed trait Option[+A]
case class Some[+A](get: A)extends Option[A]
case object None extends Option[Nothing]

object Option {
	// Exercise 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  // Exercise 4.3 
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a flatMap (aa => b map (bb => f(aa, bb)))
  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
  	a.foldRight(Some(Nil: List[A]):Option[List[A]])(map2(_, _)(_ :: _))
  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
  	a.foldRight(Some(Nil): Option[List[B]])((a, b) => map2(f(a), b)(_ :: _))


}