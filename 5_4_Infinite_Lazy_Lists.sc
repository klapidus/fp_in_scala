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

  // takeWhile via unfold
  def takeWhileUnfold(p: A => Boolean): SlackList[A] = {
    def action(state: SlackList[A]) = state match {
      case Cons(hd, tl) if p(hd()) => Some(hd(), tl())
      case _ => None
    }
    unfold(this)(action)
  }

  // zipWith via unfold
  def zipWithUnfold[B, C](that: SlackList[B])(f: (A, B) => C): SlackList[C] = {
    def action(state: (SlackList[A], SlackList[B])) = state match {
      case (Cons(hda, tla), Cons(hdb, tlb)) => Some(f(hda(), hdb()), (tla(), tlb()))
      case _ => None
    }
    unfold((this, that))(action)
  }

  // zipAll via unfold
  def zipAllUnfold[B](that: SlackList[B]): SlackList[(Option[A], Option[B])] = {
    def action(state: (SlackList[A], SlackList[B])) = state match {
      case (Cons(hda, tla), Cons(hdb, tlb)) =>
        Some((Some(hda()), Some(hdb())), (tla(), tlb()))
      case (Cons(hda, tla), _) =>
        Some((Some(hda()), None), (tla(), Empty))
      case (_, Cons(hdb, tlb)) =>
        Some((None, Some(hdb())), (Empty, tlb()))
      case _ => None
    }
    unfold((this, that))(action)
  }

  def startsWith[AA >: A](prefix: SlackList[AA]): Boolean = (this, prefix) match {
    case (Cons(hda, tla), Cons(hdb, tlb)) if hda() == hdb() => tla().startsWith(tlb())
    case (Cons(_, _), Cons(_, _)) => false
    case (Cons(_, _), Empty) => true
    case (Empty, Cons(_, _)) => false
    case (Empty, Empty) => true
  }

  // TODO: tails with unfold


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

sl1.mapUnfold(_ * 108).takeUnfold(3).toList
sl1.mapUnfold(_ * 108).takeWhileUnfold(_ < 1757).toList

val sl2: SlackList[Int] = from(7)
sl1.zipWithUnfold(sl2)(_ * _).take(5).toList

val sl3: SlackList[Int] = from(5).take(3)
val sl4: SlackList[Int] = from(105).take(7)
sl3.zipAllUnfold(sl4).toList

sl4.startsWith(sl3)

val sl5: SlackList[Int] = from(105).take(20)
sl5.startsWith(sl4)
