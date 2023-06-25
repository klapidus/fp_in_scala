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
//  TODO
//  def take(n: Int): LazyList[A] = this match {
//    case Empty => Empty
//    case Cons(hd, tl) => Cons(hd, () => tl().take(n-1))
//  }

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

// memoizing lists
// val x = Cons(() => expensive)
// def toList: List[A]