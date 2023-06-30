enum SlackList[+A]:
  case Empty
  case Cons(hd: () => A, tl: () => SlackList[A])

  def toList: List[A] = this match {
    case Empty => Nil: List[A]
    case Cons(hd, tl) => hd() :: tl().toList
  }

  def take(n: Int): SlackList[A] = this match {
    case Cons(hd, tl) if n > 0 => Cons(hd, () => tl().take(n-1))
    case _ => Empty
  }

  def headOption: Option[A] = this match {
    case Cons(hd, _) => Some(hd())
    case _ => None
  }

  def tail: SlackList[A] = this match {
    case Cons(_, tl) => tl()
    case Empty => Empty
  }

  // 5.11 - very nice one
  def unfold[A, S](state: S)
                  (f: S => Option[(A, S)]): SlackList[A] = f(state) match {
    case Some(newVal, newState) =>
      SlackList.Cons(() => newVal, () => unfold(newState)(f))
    case None => SlackList.Empty
  }

  // 5.13 - map via unfold - neat
  def mapUnfold[B](f: A => B): SlackList[B] = {
    def f1(state: SlackList[A]): Option[(B, SlackList[A])] = state match {
      case Cons(hd, tl) => Some(f(hd()), tl())
      case Empty: SlackList[A] => None
    }
    unfold(this)(f1)
  }

  // 5.13 take via unfold, first brute-force attempt : )
  def takeUnfold(n: Int): SlackList[A] = {
    def action(state: (SlackList[A], Int)): Option[(A, (SlackList[A], Int))] = {
      state match {
        case (Cons(hd, tl), n) if n > 0 => Some(hd(), (tl(), n - 1))
        case _ => None
      }
    }
    unfold((this, n))(action)
  }


val ones: SlackList[Int] = SlackList.Cons(() => 1, () => ones)
ones.take(5).toList

def continually[A](a: A): SlackList[A] =
  SlackList.Cons(() => a, () => continually(a))

def from(n: Int): SlackList[Int] =
  SlackList.Cons(() => n, () => from(n+1))

//def fibs: SlackList[Int] = {
//  def fibInt(n: Int): Int =
//    if n == 0 then 0
//    else if n == 1 then 1
//    else fibInt(n - 1) + fibInt(n - 2)
//  SlackList.Cons(() => fibInt(), () => )
//}

val sl1: SlackList[Int] = from(5)
sl1.mapUnfold(_ * 108).take(3).toList
sl1.mapUnfold(_ * 108).takeUnfold(1).toList
sl1.mapUnfold(_ * 108).takeUnfold(2).toList
sl1.mapUnfold(_ * 108).takeUnfold(3).toList
