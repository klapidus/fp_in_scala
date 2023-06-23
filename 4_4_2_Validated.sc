{

enum Validated[+E, +A]:
  case Valid(get: A)
  case Invalid(errors: List[E])

  def toEither: Either[List[E], A] =
    this match
      case Valid(a) => Right(a)
      case Invalid(es) => Left(es)

  def map2[EE >: E, B, C](b: Validated[EE, B])
                         (f: (A, B) => C): Validated[EE, C] =
    (this, b) match {
      case (Valid(a), Valid(b)) => Valid(f(a,b))
      case (Valid(_), Invalid(es)) => Invalid(es)
      case (Invalid(es), Valid(_)) => Invalid(es)
      case (Invalid(es1), Invalid(es2)) => Invalid(es1 ++ es2)
    }

object Validated:
  def traverse[E, A, B](as: List[A])
                       (f: A => Validated[E, B]): Validated[E, List[B]] = {
    val z = Valid(Nil): Validated[E, List[B]]
    as.foldRight(z)((a, acc) => f(a).map2(acc)(_ :: _))
  }

}