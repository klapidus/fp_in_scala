enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None: Option[B])

  // B is equal to A, or B is a supertype
  // note that orElse returns Option[B], and not B like getOrElse
  // this one is neat!
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    //this.map((x: B) => Some(x)).getOrElse(ob)
    // or shorter
    this.map(Some(_)).getOrElse(ob)

  // neat
  def filter(f: A => Boolean): Option[A] =
    if (this.map(f).getOrElse(false)) then this
    else None

  // redbook solution
  def filterWithFlatMap(f: A => Boolean): Option[A] =
    this.flatMap(a => if f(a) then Some(a) else None)

import Option._

val a: Option[Int] = Some(6)
val b: Option[Int] = None
a.map(_ + 107)
b.map(_ + 107)
a.orElse(b)
b.orElse(a)

Some(6).filter(_ < 5)
Some(6).filter(_ > 5)