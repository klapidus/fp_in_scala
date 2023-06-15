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
    this.map((x: B) => Some(x)).getOrElse(ob)
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

// 4.3
def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap {
    // what we need here inside is f: x: A => Option[C]
    // x => b.map( y => f(x,y))
    x =>
      b.map {
        y => f(x, y)
      }
  }

// we can re-write map2 in the following way, verbose:
def map2Alt[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  // we can think of it as a function of b: Option[B] => Option[C]
  // b.map(f2)
  // where the signature for f2 is B => C
  // again, this is a simple map -- x: Option[B] goes to res: Option[C]
  def inner(b: Option[B], externalParam: A): Option[C] =
    b.map(x => f(externalParam, x))
  // on the other hand, it is also a function of externalParam: A
  // when b is fixed, it's signature is A => Option[C]
  // so we can flat-map like this:
  //a.flatMap{
  //  (x: A) => inner(b, x)
  //}
  // or, simpler
  a.flatMap(inner(b,_))

}

// another implementation:
def map2Alt2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  // we get A map A => Option[C], so we get Option[Option[A]]
  a.map(x => b.map(y => f(x,y))).getOrElse(None)

// finally, the easiest solution with pattern matching
def map2Alt3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
  case (Some(x), Some(y)) => Some(f(x,y))
  case _ => None
}


// 4.3.
val a1: Option[Int] = Some(23)
val b1: Option[Int] = Some(85)
map2(a1, b1)((x, y) => math.pow(x + y, 2).toInt)
map2Alt(a1, b1)((x, y) => math.pow(x + y, 2).toInt)
map2Alt2(a1, b1)((x, y) => math.pow(x + y, 2).toInt)
map2Alt3(a1, b1)((x, y) => math.pow(x + y, 2).toInt)

//4.4.
// [Some(1), Some(4), Some(5)] => Some[1, 4, 5]
// [Some(1), None, Some(5)] => None
def sequence[A](as: List[Option[A]]): Option[List[A]] =
  val z = Some(List.empty[A])
  def inner(b: List[A], a: A): List[A] =
    b :+ a
  as.foldLeft(z){
    // 3 different options
    //(b, a) => map2(b, a)(inner)
    map2(_, _)(inner)
    //map2(_, _)(_ :+ _)
  }

def sequenceAlt[A](as: List[Option[A]]) =
  as.foldLeft(Some(List.empty[A])){
    map2(_, _)(_ :+ _)
  }

// this one is excessively verbose
def traverseAlt[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as.foldLeft(Some(List.empty[B])) {
    (b, a) =>
      (b, a) match {
        case (_, None) => None
        case (z, x) => map2(z, f(x))(_ :+ _)
      }
  }

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as.foldLeft(Some(List.empty[B])) {
    (z, x) => map2(z, f(x))(_ :+ _)
  }

def sequenceTraverse[A](as: List[Option[A]]): Option[List[A]] =
  traverse(as)((x: Option[A]) => x)
  // or simpler:
  //traverse(as)(a => a)


sequence(List(Some(1), Some(4), Some(5)))
sequence(List(Some(1), None, Some(5)))

sequenceTraverse(List(Some(1), Some(4), Some(5)))
sequenceTraverse(List(Some(1), None, Some(5)))

traverse(List(1, 2, 3))((x: Int) => Some(x + 108))
traverse(List(1, 2, 3))((x: Int) => if (x < 3) Some(x + 108) else None)

//Cons(h, t) is h :: t


