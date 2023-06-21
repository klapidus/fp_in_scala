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

object Person:
  def makeBoth(name: String, age: Int): Either[List[String], Person] =
    map2Both(Name(name), Age(age))(new Person(_, _))

val p1 = Person.makeBoth("kiki", 108)
val p2 = Person.makeBoth("ikik", 801)

// List is nesting, because we have Either[List[String], A]
// so the type E in map2Both is List[String]
map2Both(p1, p2)((_, _))

// define map2All, require for Either[List[E], A]
type EitherL[H, Q] = Either[List[H], Q]
def map2All[E, A, B, C](a: EitherL[E, A], b: EitherL[E, B])
                       (f: (A, B) => C): EitherL[E, C] =
  (a, b) match {
    case (Right(va), Right(vb)) => Right(f(va, vb))
    case (Left(e1), Right(_)) => Left(e1)
    case (Right(_), Left(e2)) => Left(e2)
    // this way we stay in the List() context
    case (Left(e1), Left(e2)) => Left(e1 ++ e2)
  }

val pair = map2All(p1, p2)((_, _))

// TODO
// traverse the list and collect all errors with help of map2All
def traverseAll[E, A, B](as: List[A])(f: A => Either[E, B]): Either[List[E], List[B]] =
  ???

