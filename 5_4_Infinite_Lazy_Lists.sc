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

// 5.11 - very nice one
def unfold[A, S](state: S)
                (f: S => Option[(A, S)]): SlackList[A] = f(state) match {
  case Some(newVal, newState) => SlackList.Cons(() => newVal, () => unfold(newState)(f))
  case None => SlackList.Empty
}
