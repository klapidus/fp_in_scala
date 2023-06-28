// note that Worksheet run type is "Plain"

enum LazyList[+A]:
  case Empty
  // "val params may not be call-by-name
  // () => A
  case Cons(hd: () => A, tl: () => LazyList[A])

  def headOption: Option[A] = this match {
    case Empty => None
    // force hd(), as hd is a thunk type is () => A
    case Cons(hd, _) => Some(hd())
  }
  def toList: List[A] = this match {
    case Empty => Nil: List[A]
    case Cons(hd, tl) => hd() +: tl().toList
  }

  // TODO: ugly shit, improve
  def take(n: Int): LazyList[A] = {
    if n == 1 then this match {
      case Empty => Empty
      case Cons(hd, _) => Cons(hd, () => Empty)
    }
    else this match {
      case Empty => Empty
      case Cons(hd, tl) => Cons(hd, () => tl().take(n-1))
    }
  }

  def drop(n: Int): LazyList[A] = {
    if n == 0 then this
    else this match {
      case Empty => Empty
      case Cons(hd, tl) => tl().drop(n-1)
    }
  }

  // looks pretty, RB stands for the red book solution
  def dropRB(n: Int): LazyList[A] = this match {
    case Cons(_, tl) if n > 0 => tl().dropRB(n-1)
    case _ => this
  }

  // neat
  def takeWhile(p: A => Boolean): LazyList[A] = this match {
    case Cons(hd, tl) if p(hd()) => Cons(hd, () => tl().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(hd, tl) if p(hd()) => tl().forAll(p)
    case Cons(_, _) => false
    // will return true for an empty list, which isn't nice
    case Empty => true
  }

  def forAllAlt(p: A => Boolean): Boolean = this match {
    case Cons(hd, tl) => {
      if (p(hd())) tl().forAllAlt(p) else false
    }
    // will return true for an empty list, which isn't nice
    case Empty => true
  }

  // base z is lazy z: => B
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(hd, tl) => f(hd(), tl().foldRight(z)(f))
  }

  // forAll with foldRight
  def forAllFoldR(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) & acc)

  // TODO: 5.5 takeWhile via foldRight
//  def takeWhileFoldR(p: A => Boolean): LazyList[A] = {
//    foldRight(Empty){
//      (a, acc) =>
//        if p(a) then Cons(a, acc.takeWhileFoldR(p)) else Empty
//    }
//  }

  // 5.6
  // this one is funny,
  def headOptionFoldR: Option[A] =
    foldRight(None: Option[A])((a, acc) => Some(a))

  def mapFoldR[B](f: A => B): LazyList[B] =
    foldRight(Empty: LazyList[B]){
      (a, acc) => Cons(() => f(a), () => acc)
    }

// TODO
//  def filterFoldR(p: A => Boolean): LazyList[A] =
//    foldRight(Empty: LazyList[A]){
//      (a, acc) => if p(a) then Cons(a, acc) else acc
//    }


object LazyList:
  // as: A* - variable number of arguments of type A
  def apply[A](as: A*): LazyList[A] = {
    if as.isEmpty then LazyList.Empty
    // first let's experiment without memoization
    else LazyList.Cons(() => as.head, () => apply(as.tail*))
  }

val tl = LazyList.Cons(() => 3, () => LazyList.Empty)
val ll = LazyList.Cons(() => 4, () => tl)

val ll1 = LazyList(22, 33, 44)
ll1.headOption

ll1.toList

ll1.take(1).toList
ll1.take(2).toList
ll1.take(3).toList
ll1.take(4).toList

ll1.drop(1).toList
ll1.drop(2).toList
ll1.drop(3).toList
ll1.drop(4).toList

ll1.dropRB(1).toList
ll1.dropRB(2).toList
ll1.dropRB(3).toList
ll1.dropRB(4).toList

val ll2 = LazyList(22, 33, 44, 17)
ll2.takeWhile(_ > 1).toList
ll2.takeWhile(_ > 20).toList
ll2.takeWhile(_ > 25).toList

ll2.forAll(_ < 100)
ll2.forAll(_ > 100)

val ll3 = LazyList.Empty
ll2.headOptionFoldR
ll3.headOptionFoldR