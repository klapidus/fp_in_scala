package fpinscala.monoid

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

// note the syntax, not new Monoid[String]
val stringMonoid: Monoid[String] = new:
  def combine(a1: String, a2: String) = a1 + a2
  val empty = ""

def listMonoid[A]: Monoid[List[A]] = new:
  def combine(a1: List[A], a2: List[A]) = a1 ++ a2
  val empty = Nil

def optionMonoid[A]: Monoid[Option[A]] = new:
  def combine(o1: Option[A], o2: Option[A]): Option[A] = (o1, o2) match {
    case (Some(a), None) => Some(a)
    // note that we can't do that: we have no access to type A
    // in fact this would be possible if we'd have (implicit) access to A's Monoid
    //case (Some(a), Some(b)) => Some(a + b)
    case (None, Some(b)) => Some(b)
    case (None, None) => None
  }
  // o1 orElse o2
  val empty = None

def endoMonoid[A]: Monoid[A => A] = new:
  def combine(f1: A => A, f2: A => A): A => A = f1 andThen f2
  val empty: A => A = identity

//def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as match {
//  case Nil => m.empty
  // check if this is correct
  // but this would a good example of programming to type
//  case h::t => m.combine(f(h), foldMap(t, m)(f))
//}

//def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
//  as.foldRight(m.empty)((a, acc) => m.combine(f(a), acc))

//val intAddition: Monoid[Int] = new:
//  def combine(a1: Int, a2: Int) = a1 + a2
//  val empty = 0

// note: intAdditions works with integers
// foldMap - no surprise here - folds mapped values
//val charCount = foldMap(strings, intAddition)(_.toInt)

// foldMap with implicit monoid
def foldMap[A, B](as: List[A])(f: A => B)(using m: Monoid[B]): B =
  as.foldRight(m.empty)((a, acc) => m.combine(f(a), acc))

given intAddition: Monoid[Int] with
  def combine(a1: Int, a2: Int): Int = a1 + a2
  val empty = 0

// the name for the given instance can be skipped
//given Monoid[Int] with
//  def combine(a1: Int, a2: Int): Int = a1 + a2
//  val empty = 0

//given intMultiplication: Monoid[Int] with
//  def combine(a1: Int, a2: Int): Int = a1 * a2
//  val empty = 0

val strings = List("aba", "haba", "raba")
// correct monoid is passed implicitly, an instance is fetched from the scope
val charCount = foldMap(strings)(_.length)


given productMonoid[A, B](
  using ma: Monoid[A], mb: Monoid[B]
): Monoid[(A, B)] with
  def combine(x: (A, B), y: (A, B)) = (ma.combine(x._1, y._1), mb.combine(x._2, y._2))
  val empty = (ma.empty, mb.empty)


// nice, this is again programming to type
given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
  def combine(f: A => B, g: A => B): A => B = a => mb.combine(f(a), g(a))
  val empty: A => B = a => mb.empty