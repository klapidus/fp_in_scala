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
import scala.util.control.NonFatal

def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
  try Right(x/y)
  catch case NonFatal(t) => Left(t)

safeDiv(10, 2)

//Exercise 4.7
def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
  // each element of the list is a: Either[E, A]
  // z - the start value - Right(List()): Either[E, List[A]]
  val z = Right(List.empty[A]): Either[E, List[A]]
  as.foldRight(z){
    (a, b) => a.map2(b)(_ :: _)
  }


// todo: revise
def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  // each element of the list is a: A
  // z - the start value - Right(List()): Either[E, List[B]]
  val z = Right(List.empty[B]): Either[E, List[B]]
  as.foldRight(z){
    (a, b) => f(a).map2(b)(_ :: _)
  }

sequence(List(Right(2), Right(35), Right(175)))
// ??? todo: correct
//sequence(List(Right(2), Left('NaN'), Right(175))

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

//Name("Kirill")

// note that class Person knows nothing about Either
// but we can create Option[Person] or Either[Person]
// for example with map2(...)
case class Person(name: Name, age: Age)

// this is neat
Name("Kirill").map2(Age(108))(Person(_,_))

// or, verbose:
Name("Kirill").map2(Age(108)){
  (name, age) => Person(name, age)
}

// when at least one is invalid
Name("").map2(Age(33))(Person(_, _))
Name("Kirill").map2(Age(-3))(Person(_, _))