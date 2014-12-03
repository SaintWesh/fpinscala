trait Either[+E, +A] {
	def map[B](f: A => B): Either[E, B] = this match {
		case Left(_) => this
		case Right(a) => Right(f(a))
	}
	// covariance 
	//def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = 


	//def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
	//def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
	//Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
object Either {

	def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
		as.foldRight(Right(Nil): Either[E, List[B]])(f(_).map2(_)(_ :: _))	
	def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
		traverse(es)(a => a)
}
