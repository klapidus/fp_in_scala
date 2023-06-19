enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }
  def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }
  def orElse[EE >: E, B >: A](other: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => other
    case Right(_) => this
  }
  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(a => that.map(f(a,_)))
  }


import Either.{Left, Right}

// now we accumulate errors List[E]
def map2Both[E, A, B, C](a: Either[E, A], b: Either[E, B])
                        (f: (A, B) => C): Either[List[E], C] =
  (a, b) match
    case (Right(a), Right(b)) => Right(f(a,b))
    case (Left(el), Right(_)) => Left(List(el))
    case (Right(_), Left(er)) => Left(List(er))
    case (Left(e1), Left(e2)) => Left(List(e1, e2))


case class Name private (value: String)
object Name:
  def apply(name: String): Either[String, Name] =
    if name == "" || name == null then Left("Name is empty.")
    else Right(new Name(name))

case class Age private (value: Int)
object Age:
  def apply(age: Int): Either[String, Age] =
    if age < 0 then Left("Age is out of range.")
    else Right(new Age(age))

case class Person(name: Name, age: Age)

map2Both(Name(""), Age(-7))(Person(_,_))
// neat, we accumulated two errors into
// Left(List(e1, e2))
