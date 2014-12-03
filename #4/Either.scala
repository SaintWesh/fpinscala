sealed trait Either[+E, +A] {
	def map[B](f: A => B): Either[E, B] = 
	  this match {
	    case Right(a) => Right(f(a))
	    case Left(e) => Left(e)
	  }
  // Because the return type must be a supertype of the curreent type
	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
	  this match {
	    case Left(e) => Left(e)
	    case Right(a) => f(a)
	  }
	  
  // Because the return type must be a supertype of the curreent type
	def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
	  this match {
	    case Left(_) => b
	    case Right(a) => Right(a)
	  }
  // Because the return type must be a supertype of the curreent type
	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
		for { a <- this; b1 <- b } yield f(a,b1)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
object Either {

	def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
		as.foldRight(Right(Nil): Either[E, List[B]])(f(_).map2(_)(_ :: _))	
	def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
		traverse(es)(a => a)
}